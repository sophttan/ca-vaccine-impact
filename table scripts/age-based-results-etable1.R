###################################################################################################
#Title: Direct effects of vaccination in CA age-based results
#Author: Sophia Tan
###################################################################################################

rm(list=ls())
source("configuration.R")
source("table scripts/table-functions.R")
cases <- readRDS("results/primary/primary-model-age-based-results.RDS")
hosp <- readRDS("results/hospitalizations and deaths/hosp_results_primary_model_age_based.RDS")
death <- readRDS("results/hospitalizations and deaths/death_results_primary_model_age_based.RDS")

vacc_spread <- readRDS("data/vaccination_coverage_data.RDS")

vacc_spread_54 <- (filter(vacc_spread, weeks_since_Jan2020 >= 54) %>% apply(2, mean))[3:8]
vacc_spread_59 <- (filter(vacc_spread, weeks_since_Jan2020 >= 59) %>% apply(2, mean))[3:8]
vacc_spread_67 <- (filter(vacc_spread, weeks_since_Jan2020 >= 67) %>% apply(2, mean))[3:8]

vacc_avg <- c(vacc_spread_67[1], vacc_spread_59[2:3], vacc_spread_54[4:6])

case_tbl <- make_cases_table_p_age(cases, vacc_avg[1:5], F, "b")
hosp_tbl <- make_hosp_death_tbl(hosp, vacc_avg[c(6, 2, 3, 4)], F, "b")
death_tbl <- make_hosp_death_tbl(death, vacc_avg[c(6, 2, 3, 4)], F, "b")

primarytbl <- rbind(case_tbl, hosp_tbl, death_tbl)


res_12_18 <- readRDS("results/alternative/age-based/res_12_18_sim_age.RDS")%>% filter(weeks_since_Jan2020 >= 67)
res_18_50 <- readRDS("results/alternative/age-based/res_18_50_sim_age.RDS")%>% filter(weeks_since_Jan2020 >= 59)
res_50_65 <- readRDS("results/alternative/age-based/res_50_65_sim_age.RDS")%>% filter(weeks_since_Jan2020 >= 59)
res_65 <- readRDS("results/alternative/age-based/res_65_sim_age.RDS")%>% filter(weeks_since_Jan2020 >= 54)
res_12_18$obs <- (cases%>% filter(weeks_since_Jan2020 >= 67))$`[12,18)`
res_18_50$obs <- (cases%>% filter(weeks_since_Jan2020 >= 59))$`[18,50)`
res_50_65$obs <- (cases%>% filter(weeks_since_Jan2020 >= 59))$`[50,65)`
res_65$obs <- (cases%>% filter(weeks_since_Jan2020 >= 54))$`[65,Inf)`

hosp <- readRDS("results/hospitalizations and deaths/hosp_results_alternative_model_age_based.RDS")
death <- readRDS("results/hospitalizations and deaths/death_results_alternative_model_age_based.RDS")

case_tbl <- make_cases_table_a(vacc_avg[1:5], F, "b")
hosp_tbl <- make_hosp_death_tbl(hosp, vacc_avg[c(6, 2, 3, 4)], F, "b")
death_tbl <- make_hosp_death_tbl(death, vacc_avg[c(6, 2, 3, 4)], F, "b")

alternativetbl <- rbind(case_tbl, hosp_tbl, death_tbl)

tbl2 <- read_csv("tables/overall-main-results-tabl2.csv")
tbl2 <- tbl2 %>% select(!c(3, 4))

tbl <- rbind(primarytbl[1:5,], alternativetbl[1:5,], primarytbl[6:9,], alternativetbl[6:9,],
             primarytbl[10:13,], alternativetbl[10:13,])

names(tbl)[1] <- "Outcome"

tbl <- cbind(tbl2, tbl[,2:4])

write.csv(tbl, "tables/overall-age-based-results-etable1.csv", row.names = F)
