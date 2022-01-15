###################################################################################################
#Title: Direct Effects of Vaccination in CA Primary Analysis
# Sensitivity analysis accounting for differences in age-based eligibility over time
#Author: Sophia Tan
###################################################################################################

rm(list=ls())
source("primary/primary-model-plot-functions.R")

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

beginning <- 22
ca_cases <- cases  %>% left_join(vacc, "weeks_since_Jan2020") %>%
  replace_na(list(vacc_cum_12_18 = 0, vacc_cum_18_50=0, vacc_cum_50_65=0, vacc_cum_65=0, vacc_cum=0))
ca_cases <- ca_cases %>% filter(weeks_since_Jan2020 >= beginning)


# 12-18 cases averted
vaccine <- 67
ca_cases <- ca_cases %>% mutate(color=ifelse(weeks_since_Jan2020 < vaccine, "Before vaccination",
                                             ifelse(weeks_since_Jan2020 < 74, "After vaccination - before delta",
                                                    "After vaccination - after delta")))
ca_cases$color <- factor(ca_cases$color, levels = c("Before vaccination", "After vaccination - before delta", "After vaccination - after delta"))

model1 <- glm(`[12,18)`~log(`[0,12)`), data=ca_cases %>% filter(weeks_since_Jan2020 < vaccine), "quasipoisson")
pred <- predict(model1, newdata = ca_cases %>% select(`[0,12)`), se.fit = T)
ca_cases$pred_12_18 <- exp(pred$fit)
ca_cases$lb_12_18 <- exp(pred$fit-1.96*(pred$se.fit))
ca_cases$ub_12_18 <- exp(pred$fit+1.96*(pred$se.fit))


# 18-50 cases averted
vaccine <- 59
ca_cases <- ca_cases %>% mutate(color=ifelse(weeks_since_Jan2020 < vaccine, "Before vaccination",
                                             ifelse(weeks_since_Jan2020 < 74, "After vaccination - before delta",
                                                    "After vaccination - after delta")))
ca_cases$color <- factor(ca_cases$color, levels = c("Before vaccination", "After vaccination - before delta", "After vaccination - after delta"))

model2 <- glm(`[18,50)`~log(`[0,12)`), data=ca_cases %>% filter(weeks_since_Jan2020 < vaccine), "quasipoisson")
pred <- predict(model2, newdata = ca_cases %>% select(`[0,12)`), se.fit = T)
ca_cases$pred_18_50 <- exp(pred$fit)
ca_cases$lb_18_50 <- exp(pred$fit-1.96*(pred$se.fit))
ca_cases$ub_18_50 <- exp(pred$fit+1.96*(pred$se.fit))


# 50-65 cases averted
model3 <- glm(`[50,65)`~log(`[0,12)`), data=ca_cases %>% filter(weeks_since_Jan2020 < vaccine), "quasipoisson")
pred <- predict(model3, newdata = ca_cases %>% select(`[0,12)`), se.fit = T)
ca_cases$pred_50_65 <- exp(pred$fit)
ca_cases$lb_50_65 <- exp(pred$fit-1.96*(pred$se.fit))
ca_cases$ub_50_65 <- exp(pred$fit+1.96*(pred$se.fit))


# 65+ cases averted
vaccine <- 54
ca_cases <- ca_cases %>% mutate(color=ifelse(weeks_since_Jan2020 < vaccine, "Before vaccination",
                                             ifelse(weeks_since_Jan2020 < 74, "After vaccination - before delta",
                                                    "After vaccination - after delta")))
ca_cases$color <- factor(ca_cases$color, levels = c("Before vaccination", "After vaccination - before delta", "After vaccination - after delta"))

model4 <- glm(`[65,Inf)`~log(`[0,12)`), data=ca_cases %>% filter(weeks_since_Jan2020 < vaccine), "quasipoisson")
pred <- predict(model4, newdata = ca_cases %>% select(`[0,12)`), se.fit = T)
ca_cases$pred_65 <- exp(pred$fit)
ca_cases$lb_65 <- exp(pred$fit-1.96*pred$se.fit)
ca_cases$ub_65 <- exp(pred$fit+1.96*pred$se.fit)


ca_cases <- ca_cases %>% select(!color)
ca_cases <- ca_cases %>% mutate(pred_12_18 = ifelse(pred_12_18 >= `[12,18)`, pred_12_18, `[12,18)`),
                                        lb_12_18 = ifelse(lb_12_18 >= `[12,18)`, lb_12_18, `[12,18)`),
                                        ub_12_18 = ifelse(ub_12_18 >= `[12,18)`, ub_12_18, `[12,18)`))
ca_cases <- ca_cases %>% mutate(pred_18_50 = ifelse(pred_18_50 >= `[18,50)`, pred_18_50, `[18,50)`),
                                        lb_18_50 = ifelse(lb_18_50 >= `[18,50)`, lb_18_50, `[18,50)`),
                                        ub_18_50 = ifelse(ub_18_50 >= `[18,50)`, ub_18_50, `[18,50)`))
ca_cases <- ca_cases %>% mutate(pred_50_65 = ifelse(pred_50_65 >= `[50,65)`, pred_50_65, `[50,65)`),
                                        lb_50_65 = ifelse(lb_50_65 >= `[50,65)`, lb_50_65, `[50,65)`),
                                        ub_50_65 = ifelse(ub_50_65 >= `[50,65)`, ub_50_65, `[50,65)`))
ca_cases <- ca_cases %>% mutate(pred_65 = ifelse(pred_65 >= `[65,Inf)`, pred_65, `[65,Inf)`),
                                        lb_65 = ifelse(lb_65 >= `[65,Inf)`, lb_65, `[65,Inf)`),
                                        ub_65 = ifelse(ub_65 >= `[65,Inf)`, ub_65, `[65,Inf)`))

saveRDS(ca_cases, "results/primary/primary-model-age-based-results.RDS")




