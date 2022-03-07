###################################################################################################
#Title: Direct effects of vaccination in CA primary model
#Results for sensitivity analysis - test different definitions of the unvaccinated population
#Author: Sophia Tan
###################################################################################################

rm(list=ls())
source("configuration.R")
source("table scripts/table-functions.R")
cases <- readRDS("results/primary/primary-model-unvacc-age-group.RDS")
hosp <- readRDS("results/hospitalizations and deaths/hosp_results_primary_model_unvacc_age_group.RDS")
death <- readRDS("results/hospitalizations and deaths/death_results_primary_model_unvacc_age_group.RDS")

vacc_spread <- readRDS("data/vaccination_coverage_data.RDS")
vacc_avg <- vacc_spread[,3:8] %>% apply(2, mean)

make_cases_table_p <- function(d, vacc_avg, include_prediction, type) {
  tbl <- matrix(ncol=5)
  total_obs <- 0
  total_pred <- c(0, 0, 0)
  total_averted <- c(0, 0, 0)
  for (group in 1:3) {
    if (group==1){
      subset_cases <- d %>% select(`[18,50)`, pred_18_50, lb_18_50, ub_18_50)
    } else if (group==2){
      subset_cases <- d %>% select(`[50,65)`, pred_50_65, lb_50_65, ub_50_65)
    } else{
      subset_cases <- d %>% select(`[65,Inf)`, pred_65, lb_65, ub_65)
    }

    subset_cases <- apply(subset_cases, 2, sum)
    obs <- subset_cases[1]
    pred <- subset_cases[2:4]
    averted <- pred-obs

    total_obs <- total_obs+obs
    total_pred <- total_pred + pred
    total_averted <- total_averted + averted

    reduction <- averted/pred*100
    adj_red <- (reduction/vacc_avg[group])*100
    adj_red[adj_red > 100] <- 100

    obs <- format(obs, big.mark=",", trim=T)
    pred <- format(round(pred, -1), big.mark=",", trim = T)
    averted <- format(round(averted, -1), big.mark=",", trim = T)
    reduction <- format(round(reduction))
    adj_red <- format(round(adj_red))

    tbl <- rbind(tbl,
                 c(obs, format_res(pred), format_res(averted), format_res(reduction), format_res(adj_red)))

  }

  reduction <- total_averted/total_pred*100
  adj_red <- (reduction/vacc_avg[group+1])*100
  adj_red[adj_red > 100] <- 100

  total_obs <- format(total_obs, big.mark=",", trim=T)
  total_pred <- format(round(total_pred, -1), big.mark=",", trim = T)
  total_averted <- format(round(total_averted, -1), big.mark=",", trim = T)
  reduction <- format(round(reduction))
  adj_red <- format(round(adj_red))

  tbl[1,] <- c(total_obs, format_res(total_pred), format_res(total_averted), format_res(reduction), format_res(adj_red))
  tbl <- tbl %>% as.data.frame()
  tbl$age <- c("18+", "18-49", "50-64", "65+")

  tbl <- tbl %>% select(6, 1, 2, 3, 4, 5)

  if (type=="p") {
    names(tbl) <- c("Age group (years)", "Observed COVID-19 outcome", "Predicted COVID-19 outcome (95% PI)",
                    "Averted COVID-19 outcome (95% PI)", "Relative reduction in outcome (%) (95% PI) Unadjusted", "Adjusted")
  } else if (type=="a") {
    names(tbl) <- c("Age group (years)", "Observed COVID-19 outcome", "Predicted COVID-19 outcome (95% UI)",
                    "Averted COVID-19 outcome (95% UI)", "Relative reduction in outcome (%) (95% UI) Unadjusted", "Adjusted")

  } else {
    names(tbl) <- c("Age group (years)", "Observed outcome", "Predicted outcome (95% PI or UI)",
                    "Averted outcome (95% PI or UI)", "Relative reduction (%) (95% PI or UI) Unadjusted", "Adjusted")

  }

  if (!include_prediction) {
    tbl <- tbl %>% select(1, 4, 5, 6)
  }
  tbl
}


cases_tbl <- NULL
hosp_tbl <- NULL
death_tbl <- NULL
for (vacc in cases$unvacc %>% unique()) {
  cases_tbl <- rbind(cases_tbl, make_cases_table_p(cases%>% filter(unvacc==vacc), vacc_avg[c(2, 3, 4, 6)], F, "p")[1,])
  hosp_tbl <- rbind(hosp_tbl, make_hosp_death_tbl(hosp%>% filter(unvacc==vacc)%>%select(!unvacc), vacc_avg[c(6, 2, 3, 4)], F, "p")[1,])
  death_tbl <- rbind(death_tbl, make_hosp_death_tbl(death%>% filter(unvacc==vacc)%>%select(!unvacc), vacc_avg[c(6, 2, 3, 4)], F, "p")[1,])
}

tbl <- rbind(cases_tbl, hosp_tbl, death_tbl)

tbl <- cbind(c("COVID-19 case", rep("",1),
               "COVID-19 hospitalization", rep("",1),
               "COVID-19 death", rep("",1)),
             rep(c("<18", "12-17"), 3),tbl)

names(tbl)[1:3] <- c("Outcome", "Unvaccinated population (years)", "Vaccine-eligible population (years)")

tbl

write.csv(tbl, "tables/primary_analysis_unvacc_group_etable3.csv", row.names = F)
