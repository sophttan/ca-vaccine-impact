###################################################################################################
#Title: Plotting cases and vaccination in California over time
#Author: Sophia Tan
###################################################################################################

rm(list=ls())
setwd(here::here())

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

# load in confirmed case data (weekly)
cases <- read_csv("data/ca_case_data.csv")

# load in dataset mapping weeks since January 1, 2020 to months since January 1, 2020
dates2 <- read_csv("data/weeks_months_data.csv")

# load in vaccination data
vacc <- readRDS("data/vaccination_coverage_data.RDS")

ca_data <- cases  %>% left_join(vacc, "weeks_since_Jan2020") %>%
  replace_na(list(vacc_cum_12_18 = 0, vacc_cum_18_50=0, vacc_cum_50_65=0, vacc_cum_65=0, vacc_cum=0))

# Figure 1
p <- ggplot(ca_data, aes(weeks_since_Jan2020)) +
  geom_line(aes(y=cases, color="Reported cases")) +
  geom_line(aes(y=vacc_cum_12_18*4000, color="% vaccination 12-17")) +
  geom_line(aes(y=vacc_cum_18_50*4000, color="% vaccination 18-49")) +
  geom_line(aes(y=vacc_cum_50_65*4000, color="% vaccination 50-64")) +
  geom_line(aes(y=vacc_cum_65*4000, color="% vaccination 65+")) +
  geom_vline(xintercept = 74, size=.3, lty="longdash")+
  scale_x_continuous(name = "Month", breaks=dates2$weeks, labels=dates2$month) +
  scale_y_continuous(name="Weekly no. cases", labels=comma,
                     # Add a second axis and specify its features
                     sec.axis = sec_axis(~./4000, name="Cumulative % vaccination")) +
  theme(legend.title=element_blank(), axis.text.x = element_text(angle=90),
        axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank())

p

ggsave(p, filename = "figures/overall/figures/figure-1-ca-cases-vacc-time.png", dpi=300)
