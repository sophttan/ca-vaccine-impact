###################################################################################################
#Title: Direct effects of vaccination in CA primary model
#Results for sensitivity analysis - test different vaccine thresholds
#Author: Sophia Tan
###################################################################################################

rm(list=ls())
source("configuration.R")
source("table scripts/table-functions.R")
cases <- readRDS("results/primary/primary-model-vacc-threshold.RDS")
hosp <- readRDS("results/hospitalizations and deaths/hosp_results_primary_model_vacc_threshold.RDS")
death <- readRDS("results/hospitalizations and deaths/death_results_primary_model_vacc_threshold.RDS")

vacc_spread <- readRDS("data/vaccination_coverage_data.RDS")

cases_tbl <- NULL
hosp_tbl <- NULL
death_tbl <- NULL
for (vacc in cases$vaccine %>% unique()) {
  vacc_avg <- (vacc_spread %>% filter(weeks_since_Jan2020 >= vacc))[,3:8] %>% apply(2, mean)
  cases_tbl <- rbind(cases_tbl, make_cases_table_p(cases%>% filter(vaccine==vacc), vacc_avg[1:5], F, "p")[1,])
  hosp_tbl <- rbind(hosp_tbl, make_hosp_death_tbl(hosp%>% filter(vaccine==vacc), vacc_avg[c(6, 2, 3, 4)], F, "p")[1,])
  death_tbl <- rbind(death_tbl, make_hosp_death_tbl(death%>% filter(vaccine==vacc), vacc_avg[c(6, 2, 3, 4)], F, "p")[1,])
}

tbl <- rbind(cases_tbl, hosp_tbl, death_tbl)

tbl[,1] <- rep(cases$vaccine %>% unique(),3)
names(tbl)[1] <- "Start of vaccination"

tbl <- cbind(c("COVID-19 case", rep("",5),
               "COVID-19 hospitalization", rep("",5),
               "COVID-19 death", rep("",5)),tbl)

names(tbl)[1] <- "Outcome"

tbl

write.csv(tbl, "tables/primary_analysis_vacc_threshold_etable2.csv", row.names = F)
