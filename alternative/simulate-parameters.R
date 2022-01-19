###################################################################################################
#Title: Simulate parameters for alternative analysis
#Author: Sophia Tan
###################################################################################################


rm(list=ls())
setwd(here::here())

objective.function <- function(params) {
  alpha <- params[1]
  beta <- params[2]
  
  calculated.quantiles <- qbeta(p=c(0.025, 0.975), shape1=alpha, shape2=beta)
  squared.error.quantiles <- sum((intended.quantiles - calculated.quantiles)^2)
  
  calculated.mean <- calculate.mean(alpha, beta)
  squared.error.mean <- (intended.mean - calculated.mean)^2
  
  return(squared.error.quantiles + squared.error.mean)
}

calculate.mean <- function(alpha, beta) {
  return(alpha/(alpha+beta))
}

starting.params <- c(5, 1)

# variable parameters
# fix n as number of simulations
n <- 1000

parameters <- data.frame(tr = 1:n)
optimal <- function() {
  nlm.result <- nlm(f = objective.function, p = starting.params)
  optimal.alpha <- nlm.result$estimate[1]
  optimal.beta <- nlm.result$estimate[2]
  print(c(alpha=optimal.alpha, beta=optimal.beta))
  c(optimal.alpha, optimal.beta)
} 

sample_optimal <- function(optimal, name_param, d) {
  d[[name_param]] <- rbeta(n, optimal[1], optimal[2])
  d
}

set.seed(42)
p <- matrix(runif(n*3, 0, 1), nrow = n, ncol=3) 
intended.quantiles <- c(0.55, 0.75)
intended.mean <- 0.66
opt_jj <- optimal()
parameters$jj <- qbeta(p[,1], opt_jj[1], opt_jj[2])

# delta
intended.quantiles <- c(0.31, 0.75)
intended.mean <- 0.60
opt_jj <- optimal()
parameters$jj_delta <- qbeta(p[,1], opt_jj[1], opt_jj[2])


intended.quantiles <- c(.295, .684)
intended.mean <- 0.524
opt_p1 <- optimal()
parameters$p1 <- qbeta(p[,2], opt_p1[1], opt_p1[2])

intended.quantiles <- c(0.903, 0.976)
intended.mean <- 0.95
opt_p2 <- optimal()
parameters$p2 <- qbeta(p[,2], opt_p2[1], opt_p2[2])

intended.quantiles <- c(0.67, 0.84)
intended.mean <- 0.77
opt_pwane <- optimal()
parameters$p_wane <- qbeta(p[,2], opt_pwane[1], opt_pwane[2])

# delta
intended.quantiles <- c(.295, .684)
intended.mean <- 0.524
opt_p1 <- optimal()
parameters$p1_delta <- qbeta(p[,2], opt_p1[1], opt_p1[2])

intended.quantiles <- c(0.903, 0.976)
intended.mean <- 0.95
opt_p2 <- optimal()
parameters$p2_delta <- qbeta(p[,2], opt_p2[1], opt_p2[2])

intended.quantiles <- c(0.39, 0.65)
intended.mean <- 0.53
opt_pwane <- optimal()
parameters$p_wane_delta <- qbeta(p[,2], opt_pwane[1], opt_pwane[2])


intended.quantiles <- c(0.74, 0.87)
intended.mean <- 0.82
opt_m1 <- optimal()
parameters$m1 <- qbeta(p[,3], opt_m1[1], opt_m1[2])

intended.quantiles <- c(0.89, 0.97)
intended.mean <- 0.94
opt_m2 <- optimal()
parameters$m2 <- qbeta(p[,3], opt_m2[1], opt_m2[2])

intended.quantiles <- c(0.87, 0.96)
intended.mean <- 0.92
opt_mwane <- optimal()
parameters$m_wane <- qbeta(p[,3], opt_mwane[1], opt_mwane[2])

# delta
intended.quantiles <- c(0.607, 0.865)
intended.mean <- 0.77
opt_m1 <- optimal()
parameters$m1_delta <- qbeta(p[,3], opt_m1[1], opt_m1[2])

intended.quantiles <- c(0.89, 0.97)
intended.mean <- 0.94
opt_m2 <- optimal()
parameters$m2_delta <- qbeta(p[,3], opt_m2[1], opt_m2[2])

intended.quantiles <- c(0.702, 0.866)
intended.mean <- 0.8
opt_mwane <- optimal()
parameters$m_wane_delta <- qbeta(p[,3], opt_mwane[1], opt_mwane[2])


intended.quantiles <- c(0.32, 0.62)
intended.mean <- 0.47
set.seed(10)
parameters <- sample_optimal(optimal(), "asymp_0_18", parameters)

intended.quantiles <- c(0.222, 0.44)
intended.mean <- 0.32
set.seed(11)
parameters <- sample_optimal(optimal(), "asymp_19_59", parameters)

intended.quantiles <- c(0.127, 0.294)
intended.mean <- 0.20
set.seed(12)
parameters <- sample_optimal(optimal(), "asymp_60", parameters)

parameters %>% apply(2, quantile, probs=c(0.025, 0.975))

saveRDS(parameters, "data/simulated-parameters.RDS")
