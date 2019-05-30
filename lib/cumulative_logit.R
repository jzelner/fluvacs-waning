require(brms)

titer_levels <- function(x){
  z <- sort(unique(x))
  return(log(z)[2:length(z)])
}

c_logit <- function(f, tv, d, ...) {

cumulative_logit <- custom_family(
  "cumulative_logit", dpars = "mu",
  links = c("identity"),
  type = "int",
  vars = "tv"
)

stan_funs <- "
  real cumulative_logit_lpmf(int y, real mu, vector tv) {
    return ordered_logistic_lpmf(y | mu, tv);
  }
"

stanvars <- stanvar(scode = stan_funs,
                    block = "functions") + 
  stanvar(tv, name = "tv", block = "data")

  fit <- brm(f, data = d, family = cumulative_logit, stanvars = stanvars, ...)
  return(fit)

}

log_lik_cumulative_logit <- function(i, draws) {
  
  mu <- draws$dpars$mu[, i]
  y <- draws$data$Y[i]
  tv <- draws$stanvars$tv$sdata
  
  cumulative_logit_lpmf(y, mu, tv)
  
}