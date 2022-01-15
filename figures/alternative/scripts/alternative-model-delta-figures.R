###################################################################################################
#Title: Direct Effects of Vaccination in CA Alternative Analysis
# Sensitivity Analysis Accounting for Reduced Vaccine Effectiveness Against Delta Variant
# Figures
#Author: Sophia Tan
###################################################################################################


rm(list=ls())
setwd("/mnt/projects/covid_partners/ucsf_lo")
source("Direct Effects Analysis/Main Scripts/final-scripts-122021/alternative-model-plot-functions.R")

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

cases <- read_csv("Direct Effects Analysis/Data/ca_case_data.csv")
vacc_spread <- readRDS("Direct Effects Analysis/Data/vaccination_coverage_data.RDS")
res_12_18 <- readRDS("Direct Effects Analysis/final results/res_12_18_sim_delta.RDS")%>% filter(weeks_since_Jan2020 >= 48)
res_18_50 <- readRDS("Direct Effects Analysis/final results/res_18_50_sim_delta.RDS")%>% filter(weeks_since_Jan2020 >= 48)
res_50_65 <- readRDS("Direct Effects Analysis/final results/res_50_65_sim_delta.RDS")%>% filter(weeks_since_Jan2020 >= 48)
res_65 <- readRDS("Direct Effects Analysis/final results/res_65_sim_delta.RDS")%>% filter(weeks_since_Jan2020 >= 48)

cases_time <- cases %>% filter(weeks_since_Jan2020>=48)
res_12_18$obs <- cases_time$`[12,18)`
res_18_50$obs <- cases_time$`[18,50)`
res_50_65$obs <- cases_time$`[50,65)`
res_65$obs <- cases_time$`[65,Inf)`

res_12_18$vacc <- vacc_spread$vacc_cum_12_18
res_18_50$vacc <- vacc_spread$vacc_cum_18_50
res_50_65$vacc <- vacc_spread$vacc_cum_50_65
res_65$vacc <- vacc_spread$vacc_cum_65

dates2 <- read_csv("Direct Effects Analysis/Data/weeks_months_data.csv") %>% select(!X1)

p1 <- plot_case_predictions(res_12_18) +
  geom_line(aes(y=vacc*200, color="% vaccination")) +
  geom_vline(xintercept = 74, size=.3, lty="longdash")+
  scale_y_continuous(
    labels=comma, expand = expansion(mult = c(0, .05)),
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./200, name="Cumulative % vaccination", breaks=seq(0,100,25))
  ) 

p2 <- plot_case_predictions(res_18_50) + 
  geom_line(aes(y=vacc*1800, color="% vaccination")) +
  geom_vline(xintercept = 74, size=.3, lty="longdash")+
  scale_y_continuous(
    labels=comma, expand = expansion(mult = c(0, .05)),
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./1800, name="Cumulative % vaccination", breaks=seq(0,100,25))
  ) 

p3 <- plot_case_predictions(res_50_65) + 
  geom_line(aes(y=vacc*600, color="% vaccination")) +
  geom_vline(xintercept = 74, size=.3, lty="longdash")+
  scale_y_continuous(
    labels=comma, expand = expansion(mult = c(0, .05)),
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./600, name="Cumulative % vaccination", breaks=seq(0,100,25))
  ) 

p4 <- plot_case_predictions(res_65) +
  geom_line(aes(y=vacc*350, color="% vaccination")) +
  geom_vline(xintercept = 74, size=.3, lty="longdash")+
  scale_y_continuous(
    labels=comma, expand = expansion(mult = c(0, .05)),
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./350, name="Cumulative % vaccination", breaks=seq(0,100,25))
  ) 

(p1 + theme(axis.title.x = element_blank(), axis.title.y.right = element_blank()) + labs(title="A", subtitle="12-17 years") +
    p2 + theme(axis.title.x = element_blank(), axis.title.y.left = element_blank()) + labs(title="B", subtitle="18-49 years") +
    p3 + theme(axis.title.y.right = element_blank()) + labs(title="C", subtitle="50-64 years") +
    p4 + theme(axis.title.y.left = element_blank()) + labs(title="D", subtitle="65+ years") +
    plot_layout(guides = "collect", nrow=2) & theme(legend.position = "bottom")) %>%
  ggsave(file="Direct Effects Analysis/final plots/alternative-model-delta-mc-simulation-cases.png", dpi=300, width=7, height=7.5)

(plot_relative_reduction(res_12_18) + labs(title="A", subtitle="12-17 years") + theme(axis.title.x = element_blank()) +
    plot_relative_reduction(res_18_50) + labs(title="B", subtitle="18-49 years") + theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
    plot_relative_reduction(res_50_65) + labs(title="C", subtitle="50-64 years") + 
    plot_relative_reduction(res_65)  + theme(axis.title.y = element_blank()) +labs(title="D", subtitle="65+ years") +
    plot_layout(guides = "collect") & theme(legend.position = "bottom")) %>% 
  ggsave(file="Direct Effects Analysis/final plots/alternative-model-delta-mc-simulation-relative-reduction.png", dpi=300, width=7, height=7.5)
