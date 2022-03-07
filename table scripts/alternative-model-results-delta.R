###################################################################################################
#Title: Direct effects of vaccination in CA alternative model
#Results for sensitivity analysis of alternative model - reduced vaccine effectiveness against delta variant
#Author: Sophia Tan
###################################################################################################

rm(list=ls())
source("configuration.R")
source("table scripts/table-functions.R")

cases <- read_csv("/mnt/projects/covid_partners/ucsf_lo/Direct Effects Analysis/data/ca_case_data.csv") %>%
  filter(weeks_since_Jan2020 >= 48)
res_12_18 <- readRDS("results/alternative/delta/res_12_18_sim_delta.RDS")%>% filter(weeks_since_Jan2020 >= 48)
res_18_50 <- readRDS("results/alternative/delta/res_18_50_sim_delta.RDS")%>% filter(weeks_since_Jan2020 >= 48)
res_50_65 <- readRDS("results/alternative/delta/res_50_65_sim_delta.RDS")%>% filter(weeks_since_Jan2020 >= 48)
res_65 <- readRDS("results/alternative/delta/res_65_sim_delta.RDS")%>% filter(weeks_since_Jan2020 >= 48)
res_12_18$obs <- cases$`[12,18)`
res_18_50$obs <- cases$`[18,50)`
res_50_65$obs <- cases$`[50,65)`
res_65$obs <- cases$`[65,Inf)`

hosp <- readRDS("results/hospitalizations and deaths/hosp_results_alternative_model_delta.RDS")
death <- readRDS("results/hospitalizations and deaths/death_results_alternative_model_delta.RDS")

vacc_spread <- readRDS("data/vaccination_coverage_data.RDS")

vacc_avg <- vacc_spread[,3:8] %>% apply(2, mean)

case_tbl <- make_cases_table_a(vacc_avg[1:5], F, "a")
hosp_tbl <- make_hosp_death_tbl(hosp, vacc_avg[c(6, 2, 3, 4)], F, "a")
death_tbl <- make_hosp_death_tbl(death, vacc_avg[c(6, 2, 3, 4)], F, "a")

alternativetbl <- rbind(case_tbl, hosp_tbl, death_tbl)

tbl <- cbind(c("COVID-19 case", rep("", 4),
               "COVID-19 hospitalization", rep("",3),
               "COVID-19 death", rep("",3)), alternativetbl)

names(tbl)[1] <- "Outcome"

tbl

write.csv(tbl, "tables/alternative-model-delta-results-etable5.csv", row.names = F)
