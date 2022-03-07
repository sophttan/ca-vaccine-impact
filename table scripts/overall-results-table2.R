###################################################################################################
#Title: Direct effects of vaccination in CA overall results
#Author: Sophia Tan
###################################################################################################

rm(list=ls())
source("configuration.R")
source("table scripts/table-functions.R")
cases <- readRDS("results/primary/primary-model-main-results.RDS")
hosp <- readRDS("results/hospitalizations and deaths/hosp_results_primary_model.RDS")
death <- readRDS("results/hospitalizations and deaths/death_results_primary_model.RDS")

vacc_spread <- readRDS("data/vaccination_coverage_data.RDS")

vacc_avg <- vacc_spread[,3:8] %>% apply(2, mean)


case_tbl <- make_cases_table_p(cases, vacc_avg[1:5], T, "b")
hosp_tbl <- make_hosp_death_tbl(hosp, vacc_avg[c(6, 2, 3, 4)], T, "b")
death_tbl <- make_hosp_death_tbl(death, vacc_avg[c(6, 2, 3, 4)], T, "b")

primarytbl <- rbind(case_tbl, hosp_tbl, death_tbl)


res_12_18 <- readRDS("results/alternative/main/res_12_18_sim.RDS")%>% filter(weeks_since_Jan2020 >= 48)
res_18_50 <- readRDS("results/alternative/main/res_18_50_sim.RDS")%>% filter(weeks_since_Jan2020 >= 48)
res_50_65 <- readRDS("results/alternative/main/res_50_65_sim.RDS")%>% filter(weeks_since_Jan2020 >= 48)
res_65 <- readRDS("results/alternative/main/res_65_sim.RDS")%>% filter(weeks_since_Jan2020 >= 48)
res_12_18$obs <- cases$`[12,18)`
res_18_50$obs <- cases$`[18,50)`
res_50_65$obs <- cases$`[50,65)`
res_65$obs <- cases$`[65,Inf)`

hosp <- readRDS("results/hospitalizations and deaths/hosp_results_alternative_model.RDS")
death <- readRDS("results/hospitalizations and deaths/death_results_alternative_model.RDS")

case_tbl <- make_cases_table_a(vacc_avg[1:5], T, "b")
hosp_tbl <- make_hosp_death_tbl(hosp, vacc_avg[c(6, 2, 3, 4)], T, "b")
death_tbl <- make_hosp_death_tbl(death, vacc_avg[c(6, 2, 3, 4)], T, "b")

alternativetbl <- rbind(case_tbl, hosp_tbl, death_tbl)

tbl <- cbind(c("COVID-19 case Primary model", rep("", 4), "Alternative model", rep("",4),
        "COVID-19 hospitalization Primary model", rep("",3), "Alternative model", rep("",3),
        "COVID-19 death Primary model", rep("",3), "Alternative model", rep("",3)),
      rbind(primarytbl[1:5,], alternativetbl[1:5,], primarytbl[6:9,], alternativetbl[6:9,],
      primarytbl[10:13,], alternativetbl[10:13,]))

names(tbl)[1] <- "Outcome"

tbl

write.csv(tbl, "tables/overall-main-results-table2.csv", row.names = F)
