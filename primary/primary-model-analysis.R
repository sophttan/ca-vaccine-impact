###################################################################################################
#Title: Direct Effects of Vaccination in CA Primary Analysis
#Author: Sophia Tan
###################################################################################################

rm(list=ls())
setwd(here::here())
source("figures/primary/scripts/primary-model-plot-functions.R")

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
dates2 <- read_csv("data/weeks_months_data.csv")[,2:3]

# load in vaccination data
vacc <- readRDS("data/vaccination_coverage_data.RDS")

ca_data <- cases  %>% left_join(vacc, "weeks_since_Jan2020") %>%
  replace_na(list(vacc_cum_12_18 = 0, vacc_cum_18_50=0, vacc_cum_50_65=0, vacc_cum_65=0, vacc_cum=0))




#### Primary analysis ####

beginning <- 22
vaccine <- 48

# add color to represent pre-vaccine or post-vaccine
ca_cases <- ca_data %>% mutate(color=ifelse(weeks_since_Jan2020 < vaccine, "Before vaccination",
                                             ifelse(weeks_since_Jan2020 < 74, "After vaccination - before delta",
                                                    "After vaccination - delta")))
ca_cases$color <- factor(ca_cases$color, levels = c("Before vaccination", "After vaccination - before delta", "After vaccination - delta"))

# visually check relationships between weekly cases unvaccinated group and other age groups
ca_cases <- ca_cases %>% filter(weeks_since_Jan2020 >= beginning)
calibration <- ca_cases %>% filter(weeks_since_Jan2020 < vaccine)
calibration_plot <- calibration %>% ggplot(aes(`[0,12)`, `[12,18)`)) + geom_point() +
  scale_x_continuous(name=element_blank(), label=comma) + scale_y_continuous(name = "Weekly no. cases 12-17", label=comma)+
  calibration %>% ggplot(aes(`[0,12)`, `[18,50)`)) + geom_point() +
  scale_x_continuous(name=element_blank(), label=comma) + scale_y_continuous(name = "Weekly no. cases 18-49", label=comma)+
  calibration %>% ggplot(aes(`[0,12)`, `[50,65)`)) + geom_point() +
  scale_x_continuous(name="Weekly no. cases <12", label=comma) + scale_y_continuous(name = "Weekly no. cases 50-64", label=comma)+
  calibration %>% ggplot(aes(`[0,12)`, `[65,Inf)`)) + geom_point() +
  scale_x_continuous(name="Weekly no. cases <12", label=comma) + scale_y_continuous(name = "Weekly no. cases 65+", label=comma)
calibration_plot



# 12-18 cases averted
model1 <- glm(`[12,18)`~log(`[0,12)`), data=ca_cases %>% filter(weeks_since_Jan2020 < vaccine), "quasipoisson")
pred <- predict(model1, newdata = ca_cases %>% select(`[0,12)`), se.fit = T)
ca_cases$pred_12_18 <- exp(pred$fit)
ca_cases$lb_12_18 <- exp(pred$fit-1.96*(pred$se.fit))
ca_cases$ub_12_18 <- exp(pred$fit+1.96*(pred$se.fit))

fit1 <- plot_model_fit(calibration, model1, "[0,12)", "[12,18)", "<12", "12-17")



# 18-50 cases averted
model2 <- glm(`[18,50)`~log(`[0,12)`), data=ca_cases %>% filter(weeks_since_Jan2020 < vaccine), "quasipoisson")
pred <- predict(model2, newdata = ca_cases %>% select(`[0,12)`), se.fit = T)
ca_cases$pred_18_50 <- exp(pred$fit)
ca_cases$lb_18_50 <- exp(pred$fit-1.96*(pred$se.fit))
ca_cases$ub_18_50 <- exp(pred$fit+1.96*(pred$se.fit))

fit2 <- plot_model_fit(calibration, model2, "[0,12)", "[18,50)", "<12", "18-49")


# 50-65 cases averted
model3 <- glm(`[50,65)`~log(`[0,12)`), data=ca_cases %>% filter(weeks_since_Jan2020 < vaccine), "quasipoisson")
pred <- predict(model3, newdata = ca_cases %>% select(`[0,12)`), se.fit = T)
ca_cases$pred_50_65 <- exp(pred$fit)
ca_cases$lb_50_65 <- exp(pred$fit-1.96*(pred$se.fit))
ca_cases$ub_50_65 <- exp(pred$fit+1.96*(pred$se.fit))

fit3 <- plot_model_fit(calibration, model3, "[0,12)", "[50,65)", "<12", "50-64")


# 65+ cases averted
model4 <- glm(`[65,Inf)`~log(`[0,12)`), data=ca_cases %>% filter(weeks_since_Jan2020 < vaccine), "quasipoisson")
pred <- predict(model4, newdata = ca_cases %>% select(`[0,12)`), se.fit = T)
ca_cases$pred_65 <- exp(pred$fit)
ca_cases$lb_65 <- exp(pred$fit-1.96*pred$se.fit)
ca_cases$ub_65 <- exp(pred$fit+1.96*pred$se.fit)

fit4 <- plot_model_fit(calibration, model4, "[0,12)", "[65,Inf)", "<12", "65+")


### save calibration plot
model_fit <- fit1 + labs(title="A", subtitle="12-17 years") + theme(axis.title.x = element_blank()) +
  fit2 + labs(title="B", subtitle="18-49 years") + theme(axis.title.x = element_blank()) +
  fit3 + labs(title="C", subtitle="50-64 years") +
  fit4 + labs(title="D", subtitle="65+ years") +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")
ggsave(model_fit, file="figures/primary/figures/primary-model-calibration-fit.png", dpi=300, width=7, height=7.5)



vaccine <- 48
### cases averted beginning phase 1a of vaccination
post_vaccine <- ca_cases %>% filter(weeks_since_Jan2020 >= vaccine)

post_vaccine <- post_vaccine %>% mutate(pred_12_18 = ifelse(pred_12_18 >= `[12,18)`, pred_12_18, `[12,18)`),
                                        lb_12_18 = ifelse(lb_12_18 >= `[12,18)`, lb_12_18, `[12,18)`),
                                        ub_12_18 = ifelse(ub_12_18 >= `[12,18)`, ub_12_18, `[12,18)`))
post_vaccine <- post_vaccine %>% mutate(pred_18_50 = ifelse(pred_18_50 >= `[18,50)`, pred_18_50, `[18,50)`),
                                        lb_18_50 = ifelse(lb_18_50 >= `[18,50)`, lb_18_50, `[18,50)`),
                                        ub_18_50 = ifelse(ub_18_50 >= `[18,50)`, ub_18_50, `[18,50)`))
post_vaccine <- post_vaccine %>% mutate(pred_50_65 = ifelse(pred_50_65 >= `[50,65)`, pred_50_65, `[50,65)`),
                                        lb_50_65 = ifelse(lb_50_65 >= `[50,65)`, lb_50_65, `[50,65)`),
                                        ub_50_65 = ifelse(ub_50_65 >= `[50,65)`, ub_50_65, `[50,65)`))
post_vaccine <- post_vaccine %>% mutate(pred_65 = ifelse(pred_65 >= `[65,Inf)`, pred_65, `[65,Inf)`),
                                        lb_65 = ifelse(lb_65 >= `[65,Inf)`, lb_65, `[65,Inf)`),
                                        ub_65 = ifelse(ub_65 >= `[65,Inf)`, ub_65, `[65,Inf)`))

post_vaccine <- post_vaccine %>% mutate(total_cases = rowSums(post_vaccine %>% select("[12,18)","[18,50)","[50,65)","[65,Inf)")),
                                        total_cases_pred = rowSums(post_vaccine %>% select(pred_12_18, pred_18_50, pred_50_65, pred_65)),
                                        total_cases_lb = rowSums(post_vaccine %>% select(lb_12_18, lb_18_50, lb_50_65, lb_65)),
                                        total_cases_ub = rowSums(post_vaccine %>% select(ub_12_18, ub_18_50, ub_50_65, ub_65)))

saveRDS(post_vaccine %>% select(!c(color)), "results/primary/primary-model-main-results.RDS")
