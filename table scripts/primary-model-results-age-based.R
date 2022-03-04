###################################################################################################
#Title: Direct Effects of Vaccination in CA Primary Analysis 
#Author: Sophia Tan
###################################################################################################

rm(list=ls())
setwd("/mnt/projects/covid_partners/ucsf_lo")
res <- readRDS("Direct Effects Analysis/final results/primary-model-age-based-results.RDS")

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
vacc_spread_54 <- (filter(vacc_spread, weeks_since_Jan2020 >= 54) %>% apply(2, mean))[3:8]
vacc_spread_59 <- (filter(vacc_spread, weeks_since_Jan2020 >= 59) %>% apply(2, mean))[3:8]
vacc_spread_67 <- (filter(vacc_spread, weeks_since_Jan2020 >= 67) %>% apply(2, mean))[3:8]

vacc_avg <- c(vacc_spread_67[1], vacc_spread_59[2:3], vacc_spread_54[4:6])

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
  res <- data[,2:4]-data[,1]
  
  res <- as.data.frame(cbind(res, data))
  names(res) <- c("pred_averted", "lb_averted", "ub_averted",
                  "actual_hosp", "pred_hosp", "lb_hosp", "ub_hosp")
  
  res$age <- age
  
  res <- res %>% mutate(pred_averted = ifelse(pred_averted < 0, 0, pred_averted),
                        lb_averted = ifelse(lb_averted < 0, 0, lb_averted),
                        ub_averted = ifelse(ub_averted < 0, 0, ub_averted),
                        pred_hosp = ifelse(pred_hosp < actual_hosp, actual_hosp, pred_hosp),
                        lb_hosp = ifelse(lb_hosp < actual_hosp, actual_hosp, lb_hosp),
                        ub_hosp = ifelse(ub_hosp < actual_hosp, actual_hosp, ub_hosp))
  
  res
}

groups <- (summary_data_age$age_hand_cut %>% unique())[2:5]
for (group in 1:4) {
  subset <- summary_data_age %>% filter(age_hand_cut==groups[group])
  if (group==1){
    subset_cases <- res %>% filter(weeks_since_Jan2020 >= 67)%>% select(pred_12_18, lb_12_18, ub_12_18)
    subset <- subset %>% filter(weeks_since_Jan2020 >= 67)
  } else if (group==2){
    subset_cases <- res %>% filter(weeks_since_Jan2020 >= 59) %>% select(pred_18_50, lb_18_50, ub_18_50) 
    subset <- subset %>% filter(weeks_since_Jan2020 >= 59)
  } else if (group==3) {
    subset_cases <- res %>% filter(weeks_since_Jan2020 >= 59)%>% select( pred_50_65, lb_50_65, ub_50_65) 
    subset <- subset %>% filter(weeks_since_Jan2020 >= 59)
  } else{
    subset_cases <- res %>% filter(weeks_since_Jan2020 >= 54)%>% select(pred_65, lb_65, ub_65) 
    subset <- subset %>% filter(weeks_since_Jan2020 >= 54)
  }
  
  hosp <- subset_cases * subset$hosp_rate/100
  death <- subset_cases * subset$death_rate/100
  
  cases_res <- prep_res(cbind(subset$cases, subset_cases), groups[group])
  hosp_res <- prep_res(cbind(subset$num_hosp, hosp), groups[group])
  death_res <- prep_res(cbind(subset$num_died, death), groups[group])
  
  
  averted <- apply(cases_res[,1:7], 2, sum)
  tbl_cases <- rbind(tbl_cases, averted[c(4, 5:7, 1:3)])
  total_res_cases <- rbind(total_res_cases, cases_res)
  red_cases <- rbind(red_cases, averted[1:3]/apply(subset_cases, 2, sum)*100)
  
  averted <- apply(hosp_res[,1:7], 2, sum)
  tbl_hosp <- rbind(tbl_hosp, averted[c(4, 5:7, 1:3)])
  total_res_hosp <- rbind(total_res_hosp, hosp_res)
  red_hosp <- rbind(red_hosp, averted[1:3]/apply(hosp[,1:3], 2, sum)*100)
  
  averted <- apply(death_res[,1:7], 2, sum)
  tbl_death <- rbind(tbl_death, averted[c(4, 5:7, 1:3)])
  total_res_death <- rbind(total_res_death, death_res)
  red_death <- rbind(red_death, averted[1:3]/apply(death[,1:3], 2, sum)*100)
  
}


tbl_cases <- rbind(tbl_cases, apply(tbl_cases, 2, sum))
tbl_hosp <- rbind(tbl_hosp[2:4,], apply(tbl_hosp[2:4,], 2, sum))
tbl_death <- rbind(tbl_death[2:4,], apply(tbl_death[2:4,], 2, sum))

cases <- format(round(tbl_cases, -1), big.mark=",", trim = T)
hosp <- format(round(tbl_hosp, -1), big.mark=",", trim = T)
death <- format(round(tbl_death, -1), big.mark=",", trim = T)

cases <- as.data.frame(cbind(c(groups%>% as.character(), "Total"), paste0(cases[,2], " (", cases[,3], ", ", cases[,4], ")"),
                             paste0(cases[,5], " (", cases[,6], ", ", cases[,7], ")")))
hosp <- as.data.frame(cbind(c(groups[2:4]%>% as.character(), "Total"), paste0(hosp[,2], " (", hosp[,3], ", ", hosp[,4], ")"),
                            paste0(hosp[,5], " (", hosp[,6], ", ", hosp[,7], ")")))
death <- as.data.frame(cbind(c(groups[2:4]%>% as.character(), "Total"), paste0(death[,2], " (", death[,3], ", ", death[,4], ")"),
                             paste0(death[,5], " (", death[,6], ", ", death[,7], ")")))

tbl <- rbind(cases, hosp, death)

vacc_coverage <- vacc_avg[c(1, 2, 3, 4, 5)]
red_cases <- rbind(red_cases, (tbl_cases[5,5:7]/tbl_cases[5,2:4])*100)
red_cases <- cbind(red_cases, red_cases/vacc_coverage*100)
red_cases[red_cases > 100] <- 100

vacc_coverage <- vacc_avg[c(2, 3, 4, 6)]
red_hosp <- rbind(red_hosp[2:4,], (tbl_hosp[4,5:7]/tbl_hosp[4,2:4])*100)
red_hosp <- cbind(red_hosp, red_hosp/vacc_coverage*100)
red_hosp[red_hosp > 100] <- 100

red_death <- rbind(red_death[2:4,], (tbl_death[4,5:7]/tbl_death[4,2:4])*100)
red_death <- cbind(red_death, red_death/vacc_coverage*100)
red_death[red_death > 100] <- 100

cases <- format(round(red_cases), big.mark=",", trim=T)
hosp <- format(round(red_hosp), big.mark=",", trim=T)
death <- format(round(red_death), big.mark=",", trim=T)

cases <- cbind(paste0(cases[,1], " (", cases[,2], ", ", cases[,3], ")"),
               paste0(cases[,4], " (", cases[,5], ", ", cases[,6], ")"))
hosp <- cbind(paste0(hosp[,1], " (", hosp[,2], ", ", hosp[,3], ")"),
              paste0(hosp[,4], " (", hosp[,5], ", ", hosp[,6], ")"))
death <- cbind(paste0(death[,1], " (", death[,2], ", ", death[,3], ")"),
               paste0(death[,4], " (", death[,5], ", ", death[,6], ")"))
tbl <- cbind(tbl, rbind(cases,hosp,death))
tbl <- tbl[,c(1, 3:5)]
names(tbl) <- c("Age group", 
                "Averted outcome (95% PI)",
                "Relative reduction in outcome (%) (95% PI) Unadjusted",
                "Adjusted")
tbl %>% write_csv("Direct Effects Analysis/final tables/primary_analysis_age_based_tbl.csv")
