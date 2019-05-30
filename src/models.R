require(dplyr)
require(brms)
require(readr)
require(lubridate)
require(drake)
require(forcats)
source("lib/cumulative_logit.R")
pkgconfig::set_config("drake::strings_in_dots" = "literals")

## Define functions for processing input data here
preprocess_data <- function(df) {
 
  df$SERO_DATE <- mdy(df$SERO_DATE)
  df$VAX_DATE <- mdy(df$VAX_DATE)
  
  df$VAX_LAG <- as.numeric(df$SERO_DATE - df$VAX_DATE)/28
  
  return(df)
}



separate_baseline_titer <- function(df) {
  ## First grab the first observation for everyone
  baseline_df <- df %>% 
    select(STUDYID, SERO_SPECIMEN, LOG2_HAI_H3NY) %>%
    filter(SERO_SPECIMEN == 4) %>%
    select(-SERO_SPECIMEN) %>%
    rename(BASELINE_TITER = LOG2_HAI_H3NY)
  
  ## Now, filter out the initial values from the original
  ## dataset
  analysis_df <- df %>% 
    filter(SERO_SPECIMEN > 4) %>%
     right_join(baseline_df)
  
  return(analysis_df)
}


assign_analysis_waves <- function(x, z) {
  
  ## Join data frames
  df <- inner_join(x, z) 
  
  ## Make an empty PLATE var
  df$PLATE <- rep(0, nrow(df))
  
  ## Figure out which indices of a transformed 
  ## HAI are integers
  
  raw_hai <- 4*(2**(4*df$LOG2_HAI_H3NY))
  plate_3_idx <- which(raw_hai %% 1 == 0 )
  df$CORRECTION[plate_3_idx] <- 0
  df$PLATE[df$CORRECTION > 0] <- 1
  df$PLATE[df$CORRECTION < 0] <- 2
  df$PLATE <- as.factor(df$PLATE)
  df$RAW_HAI <- round(4*(2**(df$LOG2_HAI_H3NY - df$CORRECTION)))
  df$RAW_LOG2_HAI <- log2(df$RAW_HAI/4)
  df$LOG2_HAI_F <- ordered(df$RAW_LOG2_HAI)
  df$HAI_CAT <- df$RAW_LOG2_HAI + 1
 return(df)
}


data_plan <- drake_plan(
  correction_d = read_csv(file_in("data/titer_corrections.csv")),
  fluv_d = read_csv(file_in("data/fluvacsH3NYHAI2004-2007.csv")) %>%
                     preprocess_data %>%
                     assign_analysis_waves(correction_d) %>%  
                     separate_baseline_titer %>%
                     mutate(STUDY_ARM = fct_recode(as.factor(VAXCODE), LAIV = "1", IIV = "2", PLACEBO = "3")) %>%
                     mutate(STUDY_ARM = fct_relevel(STUDY_ARM, c("PLACEBO", "IIV", "LAIV")))
                     ) 

gaussian_plan <- drake_plan(gaussian_m = brm(RAW_LOG2_HAI ~ STUDY_ARM + VAX_LAG*STUDY_ARM + ENROLLED_0405 + MALE + AGE + PLATE +(1 + VAX_LAG | STUDYID), 
                                    data = fluv_d, 
                                    family = gaussian(),
                                    chains = 4))

ordered_logit_plan <- drake_plan(
ol_m = c_logit(HAI_CAT ~ STUDY_ARM + VAX_LAG*STUDY_ARM + ENROLLED_0405 + MALE + AGE + PLATE + (1 + VAX_LAG | STUDYID), 
               titer_levels(fluv_d$RAW_HAI), fluv_d, 
               chains = 4)
)


                    
                    
analysis_plan <- rbind(data_plan, 
                       gaussian_plan, 
                       ordered_logit_plan)


