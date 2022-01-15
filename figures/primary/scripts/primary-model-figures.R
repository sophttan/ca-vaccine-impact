###################################################################################################
#Title: Direct Effects of Vaccination in CA Primary Analysis 
#Primary analysis figures
#Author: Sophia Tan
###################################################################################################

rm(list=ls())
setwd("/mnt/projects/covid_partners/ucsf_lo")
source("Direct Effects Analysis/Main Scripts/final-scripts-122021/primary-model-plot-functions.R")

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

# model 1 results
res <- readRDS("Direct Effects Analysis/final results/primary-model-main-results.RDS")

dates2 <- read_csv("Direct Effects Analysis/weeks_months_data.csv")

p1<-plot_averted_cases(res, "[12,18)") +
  geom_line(aes(y=vacc_cum_12_18*250, color="% vaccination")) +
  scale_y_continuous(name="Weekly no. cases", labels=comma,expand = expansion(mult = c(0, .05)),
                     # Add a second axis and specify its features
                     sec.axis = sec_axis(~./250, breaks=seq(0,100,25))) 

p2<- plot_averted_cases(res, "[18,50)") +
  geom_line(aes(y=vacc_cum_18_50*3000, color="% vaccination")) +
  scale_y_continuous(name= element_blank(), # "Weekly no. cases", 
                     labels=comma,expand = expansion(mult = c(0, .05)),
                     # Add a second axis and specify its features
                     sec.axis = sec_axis(~./3000, name="Cumulative % vaccination", breaks=seq(0,100,25))) 

p3<- plot_averted_cases(res, "[50,65)") +
  geom_line(aes(y=vacc_cum_50_65*800, color="% vaccination")) +
  scale_y_continuous(name="Weekly no. cases", labels=comma,expand = expansion(mult = c(0, .05)),
                     # Add a second axis and specify its features
                     sec.axis = sec_axis(~./800, breaks=seq(0,100,25))) 

p4<- plot_averted_cases(res, "[65,Inf)") + 
  geom_line(aes(y=vacc_cum_65*400, color="% vaccination")) +
  scale_y_continuous(name= element_blank(), #"Weekly no. cases", 
                     labels=comma,expand = expansion(mult = c(0, .05)),
                     # Add a second axis and specify its features
                     sec.axis = sec_axis(~./400, name="Cumulative % vaccination", breaks=seq(0,100,25)))

case_pred <- p1 + labs(title="A", subtitle="12-17 years") + theme(axis.title.x = element_blank()) +
  p2 + labs(title="B", subtitle="18-49 years") + theme(axis.title.x = element_blank()) +
  p3 + labs(title="C", subtitle="50-64 years") + 
  p4 + labs(title="D", subtitle="65+ years") +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave(case_pred, file="Direct Effects Analysis/final plots/case-based-case-pred.png", dpi=300, width=7, height=7.5)



r1 <- res %>%
  ggplot(aes(x=weeks_since_Jan2020)) +
  geom_ribbon(aes(ymax=(ub_12_18-`[12,18)`)/ub_12_18*100, ymin=(lb_12_18-`[12,18)`)/lb_12_18*100), fill="grey80") +
  geom_line(aes(y=(pred_12_18-`[12,18)`)/pred_12_18*100, color="% reduction cases")) +
  geom_line(aes(y=vacc_cum_12_18, color="% cumulative vaccination")) +
  scale_y_continuous(name = "Vaccine coverage or case reduction (%)", labels = comma) +
  scale_x_continuous(name = "Month", breaks=dates2$weeks, labels=dates2$month) +
  theme(legend.title=element_blank(), axis.text.x = element_text(angle=90),
        axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank())

r2 <- res %>%
  ggplot(aes(x=weeks_since_Jan2020)) +
  geom_ribbon(aes(ymax=(ub_18_50-`[18,50)`)/ub_18_50*100, ymin=(lb_18_50-`[18,50)`)/lb_18_50*100), fill="grey80") +
  geom_line(aes(y=(pred_18_50-`[18,50)`)/pred_18_50*100, color="% reduction cases")) +
  geom_line(aes(y=vacc_cum_18_50, color="% cumulative vaccination")) +
  scale_y_continuous(name = "Vaccine coverage or case reduction (%)", labels = comma) +
  scale_x_continuous(name = "Month", breaks=dates2$weeks, labels=dates2$month) +
  theme(legend.title=element_blank(), axis.text.x = element_text(angle=90),
        axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank())

r3 <- res %>%
  ggplot(aes(x=weeks_since_Jan2020)) +
  geom_ribbon(aes(ymax=(ub_50_65-`[50,65)`)/ub_50_65*100, ymin=(lb_50_65-`[50,65)`)/lb_50_65*100), fill="grey80") +
  geom_line(aes(y=(pred_50_65-`[50,65)`)/pred_50_65*100, color="% reduction cases")) +
  geom_line(aes(y=vacc_cum_50_65, color="% cumulative vaccination")) +
  scale_y_continuous(name = "Vaccine coverage or case reduction (%)", labels = comma) +
  scale_x_continuous(name = "Month", breaks=dates2$weeks, labels=dates2$month) +
  theme(legend.title=element_blank(), axis.text.x = element_text(angle=90),
        axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank())

r4 <- res %>%
  ggplot(aes(x=weeks_since_Jan2020)) +
  geom_ribbon(aes(ymax=(ub_65-`[65,Inf)`)/ub_65*100, ymin=(lb_65-`[65,Inf)`)/lb_65*100), fill="grey80") +
  geom_line(aes(y=(pred_65-`[65,Inf)`)/pred_65*100, color="% reduction cases")) +
  geom_line(aes(y=vacc_cum_65, color="% cumulative vaccination")) +
  scale_y_continuous(name = "Vaccine coverage or case reduction (%)", labels = comma) +
  scale_x_continuous(name = "Month", breaks=dates2$weeks, labels=dates2$month) +
  theme(legend.title=element_blank(), axis.text.x = element_text(angle=90),
        axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank())

relative_reduction <- r1 + labs(title="A", subtitle="12-17 years") + theme(axis.title.x = element_blank()) +
  r2 + labs(title="B", subtitle="18-49 years") + theme(axis.title.y=element_blank(), axis.title.x = element_blank()) +
  r3 + labs(title="C", subtitle="50-64 years") + 
  r4 + labs(title="D", subtitle="65+ years") + theme(axis.title.y=element_blank()) +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave(relative_reduction, file="Direct Effects Analysis/final plots/case-based-model-relative-reduction.png", dpi=300, width=7, height=7.5)
