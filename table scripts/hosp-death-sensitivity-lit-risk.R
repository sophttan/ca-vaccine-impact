###################################################################################################
#Title: Hospitalizations and deaths
#Results for sensitivity analysis of alternative model - literature estimates of risk of severe outcomes
#Author: Sophia Tan
###################################################################################################

rm(list=ls())
source("configuration.R")
source("src/3-hospitalizations and deaths/hosp_and_death_functions.R")
source("table scripts/table-functions.R")


vacc_spread <- readRDS("data/vaccination_coverage_data.RDS")

vacc_avg <- vacc_spread[,3:8] %>% apply(2, mean)

hosp1 <- readRDS("results/hospitalizations and deaths/hosp_results_primary_model_lit_risk.RDS")
hosp1 <- make_hosp_death_tbl(hosp1, vacc_avg[c(6, 2, 3, 4)], F, "a")

hosp2 <- readRDS("results/hospitalizations and deaths/hosp_results_alternative_model_lit_risk.RDS")
hosp2 <- make_hosp_death_tbl(hosp2, vacc_avg[c(6, 2, 3, 4)], F, "a")

death1 <- readRDS("results/hospitalizations and deaths/death_results_primary_model_lit_risk.RDS")
death1 <- make_hosp_death_tbl(death1, vacc_avg[c(6, 2, 3, 4)], F, "a")

death2 <- readRDS("results/hospitalizations and deaths/death_results_alternative_model_lit_risk.RDS")
death2 <- make_hosp_death_tbl(death2, vacc_avg[c(6, 2, 3, 4)], F, "a")

tbl <- rbind(hosp1, hosp2, death1, death2)

tbl <- cbind(c("COVID-19 hospitalization Primary model", rep("",3), "Alternative model",rep("",3),
               "COVID-19 death Primary model", rep("",3), "Alternative model", rep("",3)), tbl)

names(tbl)[1] <- "Outcome"

tbl

write.csv(tbl, "tables/hosp-death-lit-risk-etable7.csv", row.names = F)
