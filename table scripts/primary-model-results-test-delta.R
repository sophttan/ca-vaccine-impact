###################################################################################################
#Title: Direct Effects of Vaccination in CA Primary Analysis
#Results of sensitivity analysis - testing impact of changing infectiousness of delta variant
#Author: Sophia Tan
###################################################################################################

rm(list=ls())
source("configuration.R")
source("table scripts/table-functions.R")
cases <- readRDS("results/primary/primary-model-test-delta.RDS")
hosp <- readRDS("results/hospitalizations and deaths/hosp_results_primary_model_test_delta.RDS")
death <- readRDS("results/hospitalizations and deaths/death_results_primary_model_test_delta.RDS")

make_cases_table_p <- function(d, more, vacc_avg, include_prediction, type) {
  tbl <- matrix(ncol=5)
  total_obs <- 0
  total_pred <- c(0, 0, 0)
  total_averted <- c(0, 0, 0)
  for (group in 1:4) {
    if (more) {
      if (group==1){
        subset_cases <- d %>% select(`[12,18)`, more_pred_12_18, more_lb_12_18, more_ub_12_18)
      } else if (group==2){
        subset_cases <- d %>% select(`[18,50)`, more_pred_18_50, more_lb_18_50, more_ub_18_50)
      } else if (group==3){
        subset_cases <- d %>% select(`[50,65)`, more_pred_50_65, more_lb_50_65, more_ub_50_65)
      } else{
        subset_cases <- d %>% select(`[65,Inf)`, more_pred_65, more_lb_65, more_ub_65)
      }
    } else {
      if (group==1){
        subset_cases <- d %>% select(`[12,18)`, less_pred_12_18, less_lb_12_18, less_ub_12_18)
      } else if (group==2){
        subset_cases <- d %>% select(`[18,50)`, less_pred_18_50, less_lb_18_50, less_ub_18_50)
      } else if (group==3){
        subset_cases <- d %>% select(`[50,65)`, less_pred_50_65, less_lb_50_65, less_ub_50_65)
      } else{
        subset_cases <- d %>% select(`[65,Inf)`, less_pred_65, less_lb_65, less_ub_65)
      }
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
  tbl$age <- c("12+", "12-17", "18-49", "50-64", "65+")

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

# load in vaccination data
vacc <- readRDS("data/vaccination_coverage_data.RDS")
vacc_avg <- (vacc %>% apply(2, mean))[3:8]

mcase_tbl <- make_cases_table_p(cases, T, vacc_avg[1:5], F, "p")
lcase_tbl <- make_cases_table_p(cases, F, vacc_avg[1:5], F, "p")
case_tbl <- cbind(mcase_tbl, lcase_tbl[,2:4])

mhosp_tbl <- make_hosp_death_tbl(hosp %>% select(grep("more", names(.), value=T), actual, age), vacc_avg[c(6, 2, 3, 4)], F, "p")
lhosp_tbl <- make_hosp_death_tbl(hosp %>% select(grep("less", names(.), value=T), actual, age), vacc_avg[c(6, 2, 3, 4)], F, "p")
hosp_tbl <- cbind(mhosp_tbl, lhosp_tbl[,2:4])

mdeath_tbl <- make_hosp_death_tbl(death %>% select(grep("more", names(.), value=T), actual, age), vacc_avg[c(6, 2, 3, 4)], F, "p")
ldeath_tbl <- make_hosp_death_tbl(death %>% select(grep("less", names(.), value=T), actual, age), vacc_avg[c(6, 2, 3, 4)], F, "p")
death_tbl <- cbind(mdeath_tbl, ldeath_tbl[,2:4])

primarytbl <- rbind(case_tbl, hosp_tbl, death_tbl)


tbl <- cbind(c("COVID-19 case", rep("", 4),
               "COVID-19 hospitalization", rep("",3),
               "COVID-19 death", rep("",3)), primarytbl)

names(tbl)[1] <- "Outcome"
names(tbl)[c(3, 6)] <- c("Greater infectiousness of Delta variant Averted COVID-19 outcome (95% PI)",
                         "Reduced infectiousness of Delta variant Averted COVID-19 outcome (95% PI)")

tbl

write.csv(tbl, "tables/primary-model-test-delta-etable4.csv", row.names = F)
