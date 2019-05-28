require(dplyr)
require(brms)
require(readr)
require(lubridate)
require(forcats)
require(drake)
pkgconfig::set_config("drake::strings_in_dots" = "literals")

## Define functions for processing input data here
convert_dates <- function(df) {
 
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

## Load fluvacs data
plan <- drake_plan(fluv_d = read_csv(file_in("data/fluvacsH3NYHAI2004-2007.csv")) %>%
                     convert_dates %>%
                     separate_baseline_titer %>%
                     mutate(STUDY_ARM = fct_recode(as.factor(VAXCODE), LAIV = "1", IIV = "2", PLACEBO = "3")) %>%
                     mutate(STUDY_ARM = fct_relevel(STUDY_ARM, c("PLACEBO", "IIV", "LAIV"))),
  
                   gaussian_m = brm(LOG2_HAI_H3NY ~ STUDY_ARM + VAX_LAG*STUDY_ARM + ENROLLED_0405 + MALE + AGE + (1 + VAX_LAG | STUDYID), 
                                    data = d, 
                                    family = gaussian(),
                                    chains = 1))


