###################################################################################################
#Title: Direct Effects of Vaccination in CA Alternative Analysis
#Author: Sophia Tan
###################################################################################################


rm(list=ls())
setwd(here::here())
source("alternative/alternative-model-analysis-functions.R")

#Loading in libraries
library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(tidyr)
library(tibble)
library(lubridate)
library(scales)
library(stringr)
library(gridExtra)


## fixed parameters about vaccines
# estimated probabilities that individuals receive a second dose of Pfizer or Moderna based on published literature
p_second_pfizer <- .876+.045
p_second_moderna <- .887+.0405
p_no_second_pfizer <- 1-p_second_pfizer
p_no_second_moderna <- 1-p_second_moderna

# average time between doses if received a second dose
avg_time_pfizer <- 3
avg_time_moderna <- 4


parameters <- readRDS("data/simulated-parameters.RDS")
cases <- read_csv("data/ca_case_data.csv")
ca_cases_inf <- read.csv("data/ca_case_data_infection_cutoffs.csv")
jj <- readRDS("data/jj_full_data.RDS")
p_m <- readRDS("data/pfizer_moderna_full_data.RDS")
total_doses <- readRDS("data/prop_pfizer_moderna_data.RDS")
dates2 <- read_csv("data/weeks_months_data.csv") %>% select(!X1)
vacc_spread <- readRDS("data/vaccination_coverage_data.RDS")

# fill cases dataset (for estimating total infections)
symp_ca <- ca_cases_inf %>% merge(expand.grid("age_hand_cut_inf" = unique(ca_cases_inf$age_hand_cut_inf), "weeks_since_Jan2020" = 0:max(ca_cases_inf$weeks_since_Jan2020)), all.y=T) %>%
  replace_na(list(cases=0))

# vaccination dataset
vacc_data <- jj %>% full_join(p_m, by="weeks_since_Jan2020") %>% as.data.frame()
vacc_12_17 <- prep_vacc_data(1, vacc_data)
vacc_18_49 <- prep_vacc_data(2, vacc_data)
vacc_50_64 <- prep_vacc_data(3, vacc_data)
vacc_65 <- prep_vacc_data(4, vacc_data)


ca_pop <- 39538223
pop_age <- c(3168617,17024654,7452506,6528949)


prop_cases <- ca_cases_inf %>% filter(age_hand_cut_inf != "[0,12)")
prop_cases <- prop_cases %>% group_by(weeks_since_Jan2020) %>% summarise(age_hand_cut_inf = age_hand_cut_inf,
                                                                         prop_cases = cases/sum(cases))
prop_cases <- prop_cases %>% spread(age_hand_cut_inf, prop_cases, fill=0)

res_12_18 <- data.frame(weeks_since_Jan2020 = 0:max(symp_ca$weeks_since_Jan2020))
res_18_50 <- data.frame(weeks_since_Jan2020 = 0:max(symp_ca$weeks_since_Jan2020))
res_50_65 <- data.frame(weeks_since_Jan2020 = 0:max(symp_ca$weeks_since_Jan2020))
res_65 <- data.frame(weeks_since_Jan2020 = 0:max(symp_ca$weeks_since_Jan2020))

for (i in 1:nrow(parameters)) {
  params <- parameters[i,]
  print(i)
  print(params)

  protected <- make_vacc_table(params, F)
  protected_delta <- make_vacc_table(params, T)

  prop_symp <- 1-c(rep(params$asymp_0_18, 3),rep(params$asymp_19_59, 2),rep(params$asymp_60, 2))

  ca <- prep_inf_data(prop_symp, symp_ca)

  for (group in 1:4) {
    ca <- calculate_vacc(group, ca, delta = F)
  }

  ca$susceptible_under12 <- ca_pop - sum(pop_age) - c(0, cumsum(ca$total_inf_under12)[1:nrow(ca)-1])
  ca$susceptible_over12 <- ca$susceptible_12_18 + ca$susceptible_18_50 + ca$susceptible_50_65 + ca$susceptible_65


  ####### ALTERNATIVE MODELING APPROACH #######
  # In this analysis - we use case incidence in the 12+ groups to directly estimate cases in the absence of vaccination and averted cases
  # This change to the alternative model doesn't incorporate any modeling using the unvaccinated population
  #### ca cases <12 v. multiple 12+ groups ####
  # prep dataset
  ca <- ca %>% select(!grep("\\[", names(.)))
  ca_cases <- prep_data(cases, ca) %>% left_join(vacc_spread, "weeks_since_Jan2020")

  #### make case predictions in the absence of vaccination ####
  ### predictions 12-17
  ca_cases <- make_case_predictions(ca_cases, "age_2", c("[12,18)"), c(params$asymp_0_18), pop_age[1], "cases")
  res_12_18[[paste0("run", i)]] = ca_cases$cases_novacc

  ### predictions 18-49
  ca_cases <- make_case_predictions(ca_cases, "age_3", c("[18,19)", "[19,50)"), c(params$asymp_0_18, params$asymp_19_59), pop_age[2], "cases")
  res_18_50[[paste0("run", i)]] = ca_cases$cases_novacc

  ### predictions 50-64
  ca_cases <- make_case_predictions(ca_cases, "age_4", c("[50,60)", "[60,65)"), c(params$asymp_19_59, params$asymp_60), pop_age[3], "cases")
  res_50_65[[paste0("run", i)]] = ca_cases$cases_novacc

  ### predictions 65+
  ca_cases <- make_case_predictions(ca_cases, "age_5", c("[65,Inf)"), c(params$asymp_60), pop_age[4], "cases")
  res_65[[paste0("run", i)]] = ca_cases$cases_novacc
}


for (r in c("res_12_18", "res_18_50", "res_50_65", "res_65")) {
  res <- get(r) %>% select(grep("run",  names(.)))
  quantiles <- apply(res, 1, quantile, probs = c(0.025,0.975))
  res$mean <- apply(res, 1, mean)
  res$lb <- quantiles[1,]
  res$ub <- quantiles[2,]
  res$weeks_since_Jan2020 <- 0:max(ca_cases$weeks_since_Jan2020)
  res <- res %>% select(weeks_since_Jan2020, mean, lb, ub)
  assign(r, res)
}

saveRDS(res_12_18, "results/alternative/main/res_12_18_sim.RDS")
saveRDS(res_18_50, "results/alternative/main/res_18_50_sim.RDS")
saveRDS(res_50_65, "results/alternative/main/res_50_65_sim.RDS")
saveRDS(res_65, "results/alternative/main/res_65_sim.RDS")
