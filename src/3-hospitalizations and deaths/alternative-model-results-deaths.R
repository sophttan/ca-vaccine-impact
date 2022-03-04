###################################################################################################
#Title: Direct Effects of Vaccination in CA Alternative Analysis
# Results
#Author: Sophia Tan
###################################################################################################

rm(list=ls())
setwd("/mnt/projects/covid_partners/ucsf_lo")
source("Direct Effects Analysis/Hospitalizations and deaths/alternative-model-analysis-functions.R")
source("Direct Effects Analysis/Hospitalizations and deaths/alternative-model-plot-functions.R")

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

dates <- read_csv("Direct Effects Analysis/Data/weeks_months_data.csv") %>% select(!X1)

death_counts <- readRDS("Direct Effects Analysis/Hospitalizations and deaths/hosp_rate_week_month_rates.RDS") %>% 
  select(weeks_since_Jan2020, age_hand_cut, num_died) %>% spread(age_hand_cut, num_died, fill=0) 
vacc_spread <- readRDS("Direct Effects Analysis/Data/vaccination_coverage_data.RDS")
res_18_50 <- readRDS("Direct Effects Analysis/Hospitalizations and deaths/results/res_18_50_death.RDS")%>% filter(weeks_since_Jan2020 >= 48)
res_50_65 <- readRDS("Direct Effects Analysis/Hospitalizations and deaths/results/res_50_65_death.RDS")%>% filter(weeks_since_Jan2020 >= 48)
res_65 <- readRDS("Direct Effects Analysis/Hospitalizations and deaths/results/res_65_death.RDS")%>% filter(weeks_since_Jan2020 >= 48)

death_counts <- death_counts %>% filter(weeks_since_Jan2020 >= 48)
res_18_50$obs <- death_counts$`[18,50)`
res_50_65$obs <- death_counts$`[50,65)`
res_65$obs <- death_counts$`[65,Inf)`

total_cases <- c(0, 0, 0)
total_averted <- c(0, 0, 0)
tbl <- data.frame(matrix(ncol=4))

format_res <- function(v) {
  paste0(v[1], " (", v[2], ", ", v[3], ")")
}

results <- c("res_18_50", "res_50_65", "res_65")
for (i in 1:3) {
  res <- get(results[i])
  cases <- c(sum(res$mean), sum(res$lb), sum(res$ub))
  averted <- c(difference_cases(res, "obs", "mean"),
               difference_cases(res, "obs", "lb"),
               difference_cases(res, "obs", "ub"))
  reduction <- averted/cases*100 
  
  if(i == 1){
    vacc_avg <- vacc_spread$vacc_cum_18_50 %>% mean()
  } else if(i==2) {
    vacc_avg <- vacc_spread$vacc_cum_50_65 %>% mean()
  } else{
    vacc_avg <- vacc_spread$vacc_cum_65 %>% mean()
  }
  adj_reduction <- reduction/vacc_avg*100
  
  total_cases <- total_cases + cases
  total_averted <- total_averted + averted
  
  cases <- format(round(cases, -1), big.mark=",")
  averted <- format(round(averted, -1), big.mark=",")
  reduction <- round(reduction)
  adj_reduction <- round(adj_reduction)
  
  tbl <- rbind(tbl, list(format_res(cases), format_res(averted), format_res(reduction), format_res(adj_reduction)))
}

reduction <- total_averted/total_cases*100 
adj_reduction <- reduction/mean(vacc_spread$vacc_cum_18up)*100
cases <- format(round(total_cases, -2), big.mark=",")
averted <- format(round(total_averted, -2), big.mark=",")
reduction <- round(reduction)
adj_reduction <- round(adj_reduction)

tbl[1,] <- list(format_res(cases), format_res(averted), format_res(reduction), format_res(adj_reduction))
names(tbl) <- c("Predicted COVID-19 hospitalizations (95% UI)", "Averted COVID-19 outcome (95% UI)", "Relative reduction in outcome (%) (95% UI)",
                "Adjusted")
tbl[["Age group (years)"]] <- c("18+", "18-49", "50-64", "65+")
tbl <- select(tbl, 5, 2, 3, 4)
tbl

tbl %>% write_csv("Direct Effects Analysis/Hospitalizations and deaths/alternative-model-sim-death-tbl.csv")

p1 <- plot_death_predictions(res_18_50) + 
  scale_y_continuous(labels = comma, limits = c(0, max(res_65$ub)) ) + labs(title="18-49 years")

p2 <- plot_death_predictions(res_50_65) + ylim(0, max(res_65$ub)) + 
  scale_y_continuous(labels = comma, name = NULL, limits = c(0, max(res_65$ub)) ) + labs(title="50-64 years")

p3 <- plot_death_predictions(res_65) + ylim(0, max(res_65$ub)) + 
  scale_y_continuous(labels = comma, name=NULL, limits = c(0, max(res_65$ub)) ) +labs(title="65+ years")

(p1 + p2 + p3 + plot_layout(guides = "collect", nrow=1) & theme(legend.position = "bottom")) %>%
  ggsave(file="Direct Effects Analysis/Hospitalizations and deaths/alternative-model-mc-simulation-death.png", dpi=300, width=9.5, height=4)




# res_12_18 <- readRDS("Direct Effects Analysis/Hospitalizations and deaths/results/res_12_18_delta_hosp.RDS")%>% filter(weeks_since_Jan2020 >= 48)
# res_18_50 <- readRDS("Direct Effects Analysis/Hospitalizations and deaths/results/res_18_50_delta_hosp.RDS")%>% filter(weeks_since_Jan2020 >= 48)
# res_50_65 <- readRDS("Direct Effects Analysis/Hospitalizations and deaths/results/res_50_65_delta_hosp.RDS")%>% filter(weeks_since_Jan2020 >= 48)
# res_65 <- readRDS("Direct Effects Analysis/Hospitalizations and deaths/results/res_65_delta_hosp.RDS")%>% filter(weeks_since_Jan2020 >= 48)
# 
# hosp_counts <- hosp_counts %>% filter(weeks_since_Jan2020 >= 48)
# res_12_18$obs <- hosp_counts$`[12,18)`
# res_18_50$obs <- hosp_counts$`[18,50)`
# res_50_65$obs <- hosp_counts$`[50,65)`
# res_65$obs <- hosp_counts$`[65,Inf)`
# 
# total_cases <- c(0, 0, 0)
# total_averted <- c(0, 0, 0)
# tbl <- data.frame(matrix(ncol=3))
# 
# format_res <- function(v) {
#   paste0(v[1], " (", v[2], ", ", v[3], ")")
# }
# 
# for (res in c("res_18_50", "res_50_65", "res_65")) {
#   res <- get(res)
#   cases <- c(sum(res$mean), sum(res$lb), sum(res$ub))
#   averted <- c(difference_cases(res, "obs", "mean"),
#                difference_cases(res, "obs", "lb"),
#                difference_cases(res, "obs", "ub"))
#   reduction <- averted/cases*100 
#   
#   total_cases <- total_cases + cases
#   total_averted <- total_averted + averted
#   
#   cases <- format(round(cases, -1), big.mark=",")
#   averted <- format(round(averted, -1), big.mark=",")
#   reduction <- round(reduction)
#   
#   tbl <- rbind(tbl, list(format_res(cases), format_res(averted), format_res(reduction)))
# }
# 
# reduction <- total_averted/total_cases*100 
# cases <- format(round(total_cases, -2), big.mark=",")
# averted <- format(round(total_averted, -2), big.mark=",")
# reduction <- round(reduction)
# 
# tbl[1,] <- list(format_res(cases), format_res(averted), format_res(reduction))
# names(tbl) <- c("Predicted COVID-19 hospitalizations (95% UI)", "Averted hospitalizations COVID-19 (95% UI)", "Relative reduction in % (95% UI)")
# tbl[["Age group (years)"]] <- c("Total 18+", "18-49", "50-64", "65+")
# tbl <- select(tbl, 4, 1, 2, 3)
# tbl
# 
# tbl %>% write_csv("Direct Effects Analysis/Hospitalizations and deaths/alternative-model-sim-hosp-delta-tbl.csv")
# 
# 
# 
# 
# 
# res_12_18 <- readRDS("Direct Effects Analysis/final results/res_12_18_sim_delta.RDS")%>% filter(weeks_since_Jan2020 >= 48)
# res_18_50 <- readRDS("Direct Effects Analysis/final results/res_18_50_sim_delta.RDS")%>% filter(weeks_since_Jan2020 >= 48)
# res_50_65 <- readRDS("Direct Effects Analysis/final results/res_50_65_sim_delta.RDS")%>% filter(weeks_since_Jan2020 >= 48)
# res_65 <- readRDS("Direct Effects Analysis/final results/res_65_sim_delta.RDS")%>% filter(weeks_since_Jan2020 >= 48)
# cases <- read_csv("Direct Effects Analysis/Data/ca_case_data.csv") %>% filter(weeks_since_Jan2020 >= 48)
# 
# res_12_18$obs <- cases$`[12,18)`
# res_18_50$obs <- cases$`[18,50)`
# res_50_65$obs <- cases$`[50,65)`
# res_65$obs <- cases$`[65,Inf)`
# 
# total_cases <- c(0, 0, 0)
# total_averted <- c(0, 0, 0)
# tbl <- data.frame(matrix(ncol=3))
# 
# for (res in c("res_12_18", "res_18_50", "res_50_65", "res_65")) {
#   res <- get(res)
#   cases <- c(sum(res$mean), sum(res$lb), sum(res$ub))
#   averted <- c(difference_cases(res, "obs", "mean"),
#                difference_cases(res, "obs", "lb"),
#                difference_cases(res, "obs", "ub"))
#   reduction <- averted/cases*100 
#   
#   total_cases <- total_cases + cases
#   total_averted <- total_averted + averted
#   
#   cases <- format(round(cases, -1), big.mark=",")
#   averted <- format(round(averted, -1), big.mark=",")
#   reduction <- round(reduction)
#   
#   tbl <- rbind(tbl, list(format_res(cases), format_res(averted), format_res(reduction)))
# }
# 
# reduction <- total_averted/total_cases*100 
# cases <- format(round(total_cases, -1), big.mark=",")
# averted <- format(round(total_averted, -1), big.mark=",")
# reduction <- round(reduction)
# 
# tbl[1,] <- list(format_res(cases), format_res(averted), format_res(reduction))
# names(tbl) <- c("Predicted COVID-19 cases (95% UI)", "Averted cases COVID-19 (95% UI)", "Relative reduction in % (95% UI")
# tbl[["Age group (years)"]] <- c("Total 12+", "12-17", "18-49", "50-64", "65+")
# tbl <- select(tbl, 4, 1, 2, 3)
# 
# tbl %>% write_csv("Direct Effects Analysis/final tables/alternative-model-tbl-delta.csv")
# 
# rbind(res_12_18, res_18_50, res_50_65, res_65) %>% filter(weeks_since_Jan2020 >= 74) %>% apply(2, sum)
