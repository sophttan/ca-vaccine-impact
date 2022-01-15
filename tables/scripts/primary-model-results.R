###################################################################################################
#Title: Direct Effects of Vaccination in CA Primary Analysis 
#Primary analysis results
#Author: Sophia Tan
###################################################################################################
rm(list=ls())
setwd("/mnt/projects/covid_partners/ucsf_lo")

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

# main results
res <- readRDS("Direct Effects Analysis/final results/primary-model-main-results.RDS")

format_res <- function(v) {
  paste0(v[,1], " (", v[,2], ", ", v[,3], ")")
}

cases_sum <- apply(as.data.frame(res), 2, sum)
cases_sum

# total cases 12+
total_cases_over12 <- cases_sum["total_cases"]
total_cases_pred_over12 <- cases_sum["total_cases_pred"]
total_cases_lb_over12 <- cases_sum["total_cases_lb"]
total_cases_ub_over12 <- cases_sum["total_cases_ub"]

# cases averted 12+
cases_total <- c(total_cases_pred_over12, 
                 total_cases_lb_over12, 
                 total_cases_ub_over12)
averted_total <- cases_total-total_cases_over12

reduction_total <- averted_total/c(total_cases_pred_over12, total_cases_lb_over12, total_cases_ub_over12)*100
reduction_total


# averted cases and relative reduction in cases by age group
# 12-17
cases_12_18 <- c(cases_sum["pred_12_18"], cases_sum["lb_12_18"], cases_sum["ub_12_18"])
averted_12_18 <- cases_12_18 - cases_sum["[12,18)"]
reduction_12_18 <- averted_12_18/cases_12_18*100

# 18-49
cases_18_50 <- c(cases_sum["pred_18_50"], cases_sum["lb_18_50"], cases_sum["ub_18_50"])
averted_18_50 <- cases_18_50 - cases_sum["[18,50)"]
reduction_18_50 <- averted_18_50/cases_18_50*100

# 50-64
cases_50_65 <- c(cases_sum["pred_50_65"], cases_sum["lb_50_65"], cases_sum["ub_50_65"])
averted_50_65 <- cases_50_65 - cases_sum["[50,65)"]
reduction_50_65 <- averted_50_65/cases_50_65*100

# 65+
cases_65 <- c(cases_sum["pred_65"], cases_sum["lb_65"], cases_sum["ub_65"])
averted_65 <- cases_65 - cases_sum["[65,Inf)"]
reduction_65 <- averted_65/cases_65*100

observed <- format(c(sum(res$total_cases), sum(res$`[12,18)`), sum(res$`[18,50)`), sum(res$`[50,65)`), sum(res$`[65,Inf)`)), big.mark =",", trim=T)
cases <- format_res(format(round(rbind(cases_total, cases_12_18, cases_18_50, cases_50_65, cases_65), -1), big.mark=",", trim = T))
averted <- format_res(format(round(rbind(averted_total, averted_12_18, averted_18_50, averted_50_65, averted_65), -2), big.mark=",", trim = T))
reduction <- format_res(round(rbind(reduction_total, reduction_12_18, reduction_18_50, reduction_50_65, reduction_65)))


primary_results <- data.frame("Age group" = c("Total (12+)", "12-17", "18-49", "50-64", "65+"), 
                                 "Observed COVID-19 cases" = observed, 
                                 "Predicted COVID-19 cases (95% PI)" = cases, 
                                 "Averted COVID-19 cases (95% PI)" = averted,
                                 "Reduction in cases in % (95% PI)" = reduction)
write_csv(primary_results, "Direct Effects Analysis/final tables/primary_analysis_tbl.csv")



# total cases 12+ after delta introduction
post_delta <- res %>% filter(weeks_since_Jan2020 >= 74)
post_delta_sum <- apply(as.data.frame(post_delta), 2, sum)
post_delta_sum

# total cases 12+
total_cases_over12 <- post_delta_sum["total_cases"]
total_cases_pred_over12 <- post_delta_sum["total_cases_pred"]
total_cases_lb_over12 <- post_delta_sum["total_cases_lb"]
total_cases_ub_over12 <- post_delta_sum["total_cases_ub"]

# cases averted 12+
cases_total <- c(total_cases_pred_over12, 
                 total_cases_lb_over12, 
                 total_cases_ub_over12)
averted_total <- cases_total-total_cases_over12
averted_total




### age based table
res <- readRDS("Direct Effects Analysis/final results/primary-model-age-based-results.RDS")

# case reduction with different vaccine thresholds
totals_65 <- apply(res %>% filter(weeks_since_Jan2020 >= 54), 2, sum)
totals_18 <- apply(res %>% filter(weeks_since_Jan2020 >= 59), 2, sum)
totals_12 <- apply(res %>% filter(weeks_since_Jan2020 >= 67), 2, sum)

# 12-17
cases_12_18 <- c(totals_12["pred_12_18"], totals_12["lb_12_18"], totals_12["ub_12_18"])
averted_12_18 <- cases_12_18 - totals_12["[12,18)"]
reduction_12_18 <- averted_12_18/cases_12_18*100

# 18-49
cases_18_50 <- c(totals_18["pred_18_50"], totals_18["lb_18_50"], totals_18["ub_18_50"])
averted_18_50 <- cases_18_50 - totals_18["[18,50)"]
reduction_18_50 <- averted_18_50/cases_18_50*100

# 50-64
cases_50_65 <- c(totals_18["pred_50_65"], totals_18["lb_50_65"], totals_18["ub_50_65"])
averted_50_65 <- cases_50_65 - totals_18["[50,65)"]
reduction_50_65 <- averted_50_65/cases_50_65*100

# 65+
cases_65 <- c(totals_65["pred_65"], totals_65["lb_65"], totals_65["ub_65"])
averted_65 <- cases_65 - totals_65["[65,Inf)"]
reduction_65 <- averted_65/cases_65*100

cases_total <- cases_12_18 + cases_18_50 + cases_50_65 + cases_65
averted_total <- cases_total - sum(totals_12["[12,18)"], totals_18["[18,50)"], totals_18["[50,65)"], totals_65["[65,Inf)"])
reduction_total <- averted_total/cases_total*100


cases <- format_res(format(round(rbind(cases_total, cases_12_18, cases_18_50, cases_50_65, cases_65), -1), big.mark=",", trim = T))
averted <- format_res(format(round(rbind(averted_total, averted_12_18, averted_18_50, averted_50_65, averted_65), -1), big.mark=",", trim = T))
reduction <- format_res(round(rbind(reduction_total, reduction_12_18, reduction_18_50, reduction_50_65, reduction_65)))


age_based_results <- data.frame("Age group" = c("Total (12+)", "12-17", "18-49", "50-64", "65+"), 
                              "Predicted COVID-19 cases (95% PI)" = cases, 
                              "Averted COVID-19 cases (95% PI)" = averted,
                              "Reduction in cases in % (95% PI)" = reduction)
write_csv(age_based_results, "Direct Effects Analysis/final tables/primary_analysis_age_based_tbl.csv")


