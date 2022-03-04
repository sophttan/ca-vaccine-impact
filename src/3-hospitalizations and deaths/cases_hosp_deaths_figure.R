###################################################################################################
#Title: Direct Effects of Vaccination in CA Alternative Analysis
# Results
#Author: Sophia Tan
###################################################################################################

rm(list=ls())
setwd("/mnt/projects/covid_partners/ucsf_lo")
source("Direct Effects Analysis/Hospitalizations and deaths/alternative-model-analysis-functions.R")

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
vacc_spread <- readRDS("Direct Effects Analysis/Data/vaccination_coverage_data.RDS")

theme_set(theme(legend.title=element_blank(), axis.text.x = element_text(angle=90), 
                axis.line = element_line(colour = "black"),
                panel.grid.minor = element_blank()))

hosp_counts <- readRDS("Direct Effects Analysis/Hospitalizations and deaths/hosp_rate_weekly.RDS") %>% select(!age_hand_cut) %>%  group_by(weeks_since_Jan2020) %>% summarise_all(sum)
hosp_counts <- hosp_counts %>% left_join(vacc_spread, "weeks_since_Jan2020") %>% replace_na(list(vacc_cum=0))
scalar1 <- max(hosp_counts$cases)/max(hosp_counts$vacc_cum)
cases <- hosp_counts %>% ggplot(aes(weeks_since_Jan2020, cases)) + geom_line(aes(color="Outcome")) +
  geom_line(aes(y=vacc_cum*scalar1, color="Cumulative vaccination (%)")) + 
  geom_vline(xintercept = 74, size=.3, lty="longdash") +
  scale_x_continuous(name = "Month", breaks=dates$weeks, labels=dates$month) +
  scale_y_continuous(labels=comma, name = "Weekly outcome",
                     sec.axis = sec_axis(~./scalar1, breaks=seq(0,100,25))) + 
  labs(title="A", subtitle="Cases")

scalar2 <- max(hosp_counts$hosp)/max(hosp_counts$vacc_cum)
hosp <- hosp_counts %>% ggplot(aes(weeks_since_Jan2020, hosp)) + geom_line(aes(color="Outcome")) +
  scale_x_continuous(name = "Month", breaks=dates$weeks, labels=dates$month) +
  geom_line(aes(x=weeks_since_Jan2020, y=vacc_cum*scalar2, color="Cumulative vaccination (%)")) + 
  geom_vline(xintercept = 74, size=.3, lty="longdash") +
  scale_y_continuous(labels=comma, name=NULL,
                     sec.axis = sec_axis(~./scalar2, breaks=seq(0,100,25))) +
  labs(title="B", subtitle="Hospitalizations")

scalar3 <- max(hosp_counts$died)/max(hosp_counts$vacc_cum)
died <- hosp_counts %>% ggplot(aes(weeks_since_Jan2020, died)) + geom_line(aes(color="Outcome")) +
  scale_x_continuous(name = "Month", breaks=dates$weeks, labels=dates$month) +
  geom_vline(xintercept = 74, size=.3, lty="longdash") +
  geom_line(aes(x=weeks_since_Jan2020, y=vacc_cum*scalar3, color="Cumulative vaccination (%)")) + 
  scale_y_continuous(labels=comma, name=NULL,
                     sec.axis = sec_axis(~./scalar3, breaks=seq(0,100,25), name="Cumulative vaccine coverage (%)")) +
  labs(title = "C", subtitle="Deaths")

(cases+hosp+died+
  plot_layout(guides = "collect", nrow=1) & theme(legend.position = "bottom")) %>% ggsave(filename="Direct Effects Analysis/Hospitalizations and deaths/obs_cases_hosp_deaths.jpg", dpi=300, width = 10, height=4)

library(devEMF)
emf(file="Direct Effects Analysis/final plots/obs_cases_hosp_deaths.emf", width = 10, height=4)
(cases+hosp+died+
    plot_layout(guides = "collect", nrow=1) & theme(legend.position = "bottom"))
dev.off()