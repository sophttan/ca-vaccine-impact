###################################################################################################
#Title: Direct effects of vaccination in CA alternative model
#Results for sensitivity analysis of alternative model - reduced vaccine effectiveness against delta variant
#Author: Sophia Tan
###################################################################################################

rm(list=ls())
source("configuration.R")
source("src/3-hospitalizations and deaths/hosp_and_death_functions.R")
source("table scripts/table-functions.R")

hosp_data <- readRDS("data/hosp_death_rate_week_month_rates.RDS") %>% filter(weeks_since_Jan2020 >=48)
res_18_50 <- readRDS("results/hospitalizations and deaths/hosp-simulation/res_18_50_hosp.RDS") %>% filter(weeks_since_Jan2020 >=48)
res_50_65 <- readRDS("results/hospitalizations and deaths/hosp-simulation/res_50_65_hosp.RDS") %>% filter(weeks_since_Jan2020 >=48)
res_65 <- readRDS("results/hospitalizations and deaths/hosp-simulation/res_65_hosp.RDS") %>% filter(weeks_since_Jan2020 >=48)

groups<-hosp_data[1:5,]$age_hand_cut[3:5]
results <- c("res_18_50", "res_50_65", "res_65")
total_res_hosp <- NULL
total_res_death <- NULL

for (i in 1:3) {
  res <- get(results[i])
  group <- groups[i]

  subset <- hosp_data %>% filter(age_hand_cut==group)

  hosp <- res[,2:4]
  death <- res[,2:4]
  hosp$obs <- subset$num_hosp

  hosp_res <- prep_res(hosp, group)

  total_res_hosp <- rbind(total_res_hosp, hosp_res)

}


vacc_spread <- readRDS("data/vaccination_coverage_data.RDS")

vacc_avg <- vacc_spread[,3:8] %>% apply(2, mean)

hosp <- make_hosp_death_tbl(total_res_hosp, vacc_avg[c(6, 2, 3, 4)], F, "a")



res_18_50 <- readRDS("results/hospitalizations and deaths/death-simulation/res_18_50_death.RDS") %>% filter(weeks_since_Jan2020 >=48)
res_50_65 <- readRDS("results/hospitalizations and deaths/death-simulation/res_50_65_death.RDS") %>% filter(weeks_since_Jan2020 >=48)
res_65 <- readRDS("results/hospitalizations and deaths/death-simulation/res_65_death.RDS") %>% filter(weeks_since_Jan2020 >=48)

total_res_death <- NULL

for (i in 1:3) {
  res <- get(results[i])
  group <- groups[i]

  subset <- hosp_data %>% filter(age_hand_cut==group)

  death <- res[,2:4]
  death$obs <- subset$num_died

  death_res <- prep_res(death, group)

  total_res_death <- rbind(total_res_death, death_res)

}

death <- make_hosp_death_tbl(total_res_death, vacc_avg[c(6, 2, 3, 4)], F, "a")


tbl <- rbind(hosp, death)

tbl <- cbind(c("COVID-19 hospitalization", rep("",3),
               "COVID-19 death", rep("",3)), tbl)

names(tbl)[1] <- "Outcome"

tbl

write.csv(tbl, "tables/hosp-death-simulation-results-etable7.csv", row.names = F)
