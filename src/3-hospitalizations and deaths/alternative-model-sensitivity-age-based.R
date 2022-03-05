###################################################################################################
#Title: Hospitalization and deaths from alternative model of averted cases
# Sensitivity analysis accounting for age-based eligibility over time
#Author: Sophia Tan
###################################################################################################

rm(list=ls())
source("configuration.R")
source("src/3-hospitalizations and deaths/hosp_and_death_functions.R")

dates <- read_csv("data/weeks_months_data.csv")

hosp_data <- readRDS("data/hosp_death_rate_week_month_rates.RDS") %>% filter(weeks_since_Jan2020 >=48)
res_12_18 <- readRDS("results/alternative/age-based/res_12_18_sim_age.RDS") %>% filter(weeks_since_Jan2020>=48)
res_18_50 <- readRDS("results/alternative/age-based/res_18_50_sim_age.RDS") %>% filter(weeks_since_Jan2020>=48)
res_50_65 <- readRDS("results/alternative/age-based/res_50_65_sim_age.RDS") %>% filter(weeks_since_Jan2020>=48)
res_65 <- readRDS("results/alternative/age-based/res_65_sim_age.RDS") %>% filter(weeks_since_Jan2020>=48)

groups<-hosp_data[1:5,]$age_hand_cut[3:5]
results <- c("res_18_50", "res_50_65", "res_65")

total_res_hosp <- NULL
total_res_death <- NULL

for (i in 1:3) {
  res <- get(results[i])
  group <- groups[i]
  subset <- hosp_data %>% filter(age_hand_cut==group)

  if (i == 1) {
    res <- filter(res, weeks_since_Jan2020 >= 59)
    subset <- filter(subset, weeks_since_Jan2020 >= 59)
  } else if (i == 2) {
    res <- filter(res, weeks_since_Jan2020 >= 59)
    subset <- filter(subset, weeks_since_Jan2020 >= 59)
  } else {
    res <- filter(res, weeks_since_Jan2020 >= 54)
    subset <- filter(subset, weeks_since_Jan2020 >= 54)
  }

  hosp <- (subset$hosp_rate/100) * res[,2:4]
  death <- (subset$death_rate/100) * res[,2:4]
  hosp$obs <- subset$num_hosp
  death$obs <- subset$num_died

  hosp_res <- prep_res(hosp, group)
  death_res <- prep_res(death, group)

  total_res_hosp <- rbind(total_res_hosp, hosp_res)
  total_res_death <- rbind(total_res_death, death_res)

}

saveRDS(total_res_death, "results/hospitalizations and deaths/death_results_alternative_model_age_based.RDS")
saveRDS(total_res_hosp, "results/hospitalizations and deaths/hosp_results_alternative_model_age_based.RDS")
