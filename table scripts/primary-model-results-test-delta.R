###################################################################################################
#Title: Direct Effects of Vaccination in CA Primary Analysis 
#Author: Sophia Tan
###################################################################################################

rm(list=ls())
setwd("/mnt/projects/covid_partners/ucsf_lo")
res <- readRDS("Direct Effects Analysis/final results/primary-model-test-delta.RDS")

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
library(patchwork)

# load in vaccination data
vacc <- readRDS("Direct Effects Analysis/Data/vaccination_coverage_data.RDS")
vacc_avg <- (vacc %>% apply(2, mean))[3:8]

summary_data_age <- readRDS("Direct Effects Analysis/Hospitalizations and deaths/hosp_rate_week_month_rates.RDS") %>% filter(weeks_since_Jan2020 >= 48)

total_res_cases <- NULL
tbl_cases <- NULL
red_cases <- NULL

total_res_hosp <- NULL
tbl_hosp <- NULL
red_hosp <- NULL

total_res_death <- NULL
tbl_death <- NULL
red_death <- NULL

prep_res <- function(data, age) {
  res <- data[,2:7]-data[,1]
  
  res <- as.data.frame(cbind(res, data))
  names(res) <- c("pred_averted_more", "lb_averted_more", "ub_averted_more",
                  "pred_averted_less", "lb_averted_less", "ub_averted_less",
                  "actual", "pred_more", "lb_more", "ub_more",
                  "pred_less", "lb_less", "ub_less")
  
  res$weeks_since_Jan2020 <- 48:93
  res$age <- age
  
  res <- res %>% mutate(pred_averted_more = ifelse(pred_averted_more < 0, 0, pred_averted_more),
                        lb_averted_more = ifelse(lb_averted_more < 0, 0, lb_averted_more),
                        ub_averted_more = ifelse(ub_averted_more < 0, 0, ub_averted_more),
                        pred_more = ifelse(pred_more < actual, actual, pred_more),
                        lb_more = ifelse(lb_more < actual, actual, lb_more),
                        ub_more = ifelse(ub_more < actual, actual, ub_more),
                        pred_averted_less = ifelse(pred_averted_less < 0, 0, pred_averted_less),
                        lb_averted_less = ifelse(lb_averted_less < 0, 0, lb_averted_less),
                        ub_averted_less = ifelse(ub_averted_less < 0, 0, ub_averted_less),
                        pred_less = ifelse(pred_less < actual, actual, pred_more),
                        lb_less = ifelse(lb_less < actual, actual, lb_less),
                        ub_less = ifelse(ub_less < actual, actual, ub_less))
  
  res
}

groups <- (summary_data_age$age_hand_cut %>% unique())[2:5]
for (group in 1:4) {
  subset <- summary_data_age_vacc %>% filter(age_hand_cut==groups[group])
  if (group==1){
    subset_cases <- res %>% select(more_pred_12_18, more_lb_12_18, more_ub_12_18,
                                             less_pred_12_18, less_lb_12_18, less_ub_12_18)
  } else if (group==2){
    subset_cases <- res %>% select(more_pred_18_50, more_lb_18_50, more_ub_18_50,
                                             less_pred_18_50, less_lb_18_50, less_ub_18_50)
  } else if (group==3) {
    subset_cases <- res %>% select(more_pred_50_65, more_lb_50_65, more_ub_50_65,
                                             less_pred_50_65, less_lb_50_65, less_ub_50_65)
  } else{
    subset_cases <- res %>% select(more_pred_65, more_lb_65, more_ub_65,
                                             less_pred_65, less_lb_65, less_ub_65)
  }
  
  
  hosp <- subset_cases * subset$hosp_rate/100
  death <- subset_cases * subset$death_rate/100
  
  cases_res <- prep_res(cbind(subset$cases, subset_cases), groups[group])
  hosp_res <- prep_res(cbind(subset$num_hosp, hosp), groups[group])
  death_res <- prep_res(cbind(subset$num_died, death), groups[group])
  

  averted <- apply(cases_res[,1:13], 2, sum)
  tbl_cases <- rbind(tbl_cases, averted[c(1:6, 8:13)])
  total_res_cases <- rbind(total_res_cases, cases_res)
  red_cases <- rbind(red_cases, averted[1:6]/apply(subset_cases, 2, sum)*100)
  
  averted <- apply(hosp_res[,1:13], 2, sum)
  tbl_hosp <- rbind(tbl_hosp, averted[c(1:6, 8:13)])
  total_res_hosp <- rbind(total_res_hosp, hosp_res)
  red_hosp <- rbind(red_hosp, averted[1:6]/averted[8:13]*100)
  
  averted <- apply(death_res[,1:13], 2, sum)
  tbl_death <- rbind(tbl_death, averted[c(1:6, 8:13)])
  total_res_death <- rbind(total_res_death, death_res)
  red_death <- rbind(red_death, averted[1:6]/averted[8:13]*100)
  
}


tbl_cases <- rbind(tbl_cases, apply(tbl_cases, 2, sum))
tbl_hosp <- rbind(tbl_hosp[2:4,], apply(tbl_hosp[2:4,], 2, sum))
tbl_death <- rbind(tbl_death[2:4,], apply(tbl_death[2:4,], 2, sum))

cases <- format(round(tbl_cases, -1), big.mark=",", trim = T)
hosp <- format(round(tbl_hosp, -1), big.mark=",", trim = T)
death <- format(round(tbl_death, -1), big.mark=",", trim = T)

cases <- as.data.frame(cbind(c(groups%>% as.character(), "Total"), paste0(cases[,1], " (", cases[,2], ", ", cases[,3], ")"),
                             paste0(cases[,4], " (", cases[,5], ", ", cases[,6], ")")))
hosp <- as.data.frame(cbind(c(groups[2:4]%>% as.character(), "Total"), paste0(hosp[,1], " (", hosp[,2], ", ", hosp[,3], ")"),
                            paste0(hosp[,4], " (", hosp[,5], ", ", hosp[,6], ")")))
death <- as.data.frame(cbind(c(groups[2:4]%>% as.character(), "Total"), paste0(death[,1], " (", death[,2], ", ", death[,3], ")"),
                             paste0(death[,4], " (", death[,5], ", ", death[,6], ")")))

tbl <- rbind(cases, hosp, death)

vacc_coverage <- vacc_avg[c(1, 2, 3, 4, 5)]
red_cases <- rbind(red_cases, (tbl_cases[5,1:6]/tbl_cases[5,7:12])*100)
red_cases <- cbind(red_cases, red_cases/vacc_coverage*100)
red_cases[red_cases > 100] <- 100

vacc_coverage <- vacc_avg[c(2, 3, 4, 6)]
red_hosp <- rbind(red_hosp[2:4,], (tbl_hosp[4,1:6]/tbl_hosp[4,7:12])*100)
red_hosp <- cbind(red_hosp, red_hosp/vacc_coverage*100)
red_hosp[red_hosp > 100] <- 100

red_death <- rbind(red_death[2:4,], (tbl_death[4,1:6]/tbl_death[4,7:12])*100)
red_death <- cbind(red_death, red_death/vacc_coverage*100)
red_death[red_death > 100] <- 100

cases <- format(round(red_cases), big.mark = ",", trim=T)
hosp <- format(round(red_hosp), big.mark=",", trim=T)
death <- format(round(red_death), big.mark=",", trim=T)
cases <- cbind(paste0(cases[,1], " (", cases[,2], ", ", cases[,3], ")"),
               paste0(cases[,7], " (", cases[,8], ", ", cases[,9], ")"),
               paste0(cases[,4], " (", cases[,5], ", ", cases[,6], ")"),
               paste0(cases[,10], " (", cases[,11], ", ", cases[,12], ")"))
hosp <- cbind(paste0(hosp[,1], " (", hosp[,2], ", ", hosp[,3], ")"),
               paste0(hosp[,7], " (", hosp[,8], ", ", hosp[,9], ")"),
               paste0(hosp[,4], " (", hosp[,5], ", ", hosp[,6], ")"),
               paste0(hosp[,10], " (", hosp[,11], ", ", hosp[,12], ")"))
death <- cbind(paste0(death[,1], " (", death[,2], ", ", death[,3], ")"),
               paste0(death[,7], " (", death[,8], ", ", death[,9], ")"),
               paste0(death[,4], " (", death[,5], ", ", death[,6], ")"),
               paste0(death[,10], " (", death[,11], ", ", death[,12], ")"))
tbl <- cbind(tbl, rbind(cases,hosp,death))
tbl <- select(tbl, c(1:2, 4:5, 3, 6:7))
names(tbl) <- c("Age group", 
                "Greater infectiousness of delta Averted outcome (95% PI)",
                "Relative reduction in outcome (%) (95% PI) Unadjusted",
                "Adjusted", 
                "Reduced infectiousness of delta Averted outcome (95% PI)", 
                "Relative reduction in outcome (%) (95% PI) Unadjusted",
                "Adjusted")
tbl %>% write_csv("Direct Effects Analysis/final tables/primary_analysis_test_delta_tbl.csv")
