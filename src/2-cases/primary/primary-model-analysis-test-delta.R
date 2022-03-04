###################################################################################################
#Title: Direct Effects of Vaccination in CA Primary Analysis
# Sensitivity analysis - testing increased/decreased
# infectiousness of delta variant in children <12 years
#Author: Sophia Tan
###################################################################################################

rm(list=ls())
source("configuration.R")
source("figure scripts/plot-functions.R")

# load in confirmed case data (weekly)
cases <- read_csv("/mnt/projects/covid_partners/ucsf_lo/Direct Effects Analysis/Data/ca_case_data.csv")

# load in dataset mapping weeks since January 1, 2020 to months since January 1, 2020
dates2 <- read_csv("data/weeks_months_data.csv")

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

# delta variant introduced at week 74 since 1/1/20
# if delta variant is 10% more infectious in unvaccinated population (<12), expect 10% fewer cases in unvaccinated population
# if delta variant is 10% less infectious in unvaccinated population, expect 10% more infections in unvaccinated pop
ca_cases <- ca_cases %>% mutate(more_inf_0_12 = ifelse(weeks_since_Jan2020 >= 74, `[0,12)`*.9, `[0,12)`),
                                less_inf_0_12 = ifelse(weeks_since_Jan2020 >= 74, `[0,12)`*1.1, `[0,12)`))

# 12-18 cases averted
model1 <- glm(`[12,18)`~log(more_inf_0_12), data=ca_cases %>% filter(weeks_since_Jan2020 < vaccine), "quasipoisson")
pred <- predict(model1, newdata = ca_cases %>% select(more_inf_0_12), se.fit = T)
ca_cases$more_pred_12_18 <- exp(pred$fit)
ca_cases$more_lb_12_18 <- exp(pred$fit-1.96*(pred$se.fit))
ca_cases$more_ub_12_18 <- exp(pred$fit+1.96*(pred$se.fit))

model1 <- glm(`[12,18)`~log(less_inf_0_12), data=ca_cases %>% filter(weeks_since_Jan2020 < vaccine), "quasipoisson")
pred <- predict(model1, newdata = ca_cases %>% select(less_inf_0_12), se.fit = T)
ca_cases$less_pred_12_18 <- exp(pred$fit)
ca_cases$less_lb_12_18 <- exp(pred$fit-1.96*(pred$se.fit))
ca_cases$less_ub_12_18 <- exp(pred$fit+1.96*(pred$se.fit))



# 18-50 cases averted
model2 <- glm(`[18,50)`~log(more_inf_0_12), data=ca_cases %>% filter(weeks_since_Jan2020 < vaccine), "quasipoisson")
pred <- predict(model2, newdata = ca_cases %>% select(more_inf_0_12), se.fit = T)
ca_cases$more_pred_18_50 <- exp(pred$fit)
ca_cases$more_lb_18_50 <- exp(pred$fit-1.96*(pred$se.fit))
ca_cases$more_ub_18_50 <- exp(pred$fit+1.96*(pred$se.fit))

model2 <- glm(`[18,50)`~log(less_inf_0_12), data=ca_cases %>% filter(weeks_since_Jan2020 < vaccine), "quasipoisson")
pred <- predict(model2, newdata = ca_cases %>% select(less_inf_0_12), se.fit = T)
ca_cases$less_pred_18_50 <- exp(pred$fit)
ca_cases$less_lb_18_50 <- exp(pred$fit-1.96*(pred$se.fit))
ca_cases$less_ub_18_50 <- exp(pred$fit+1.96*(pred$se.fit))



# 50-65 cases averted
model3 <- glm(`[50,65)`~log(more_inf_0_12), data=ca_cases %>% filter(weeks_since_Jan2020 < vaccine), "quasipoisson")
pred <- predict(model3, newdata = ca_cases %>% select(more_inf_0_12), se.fit = T)
ca_cases$more_pred_50_65 <- exp(pred$fit)
ca_cases$more_lb_50_65 <- exp(pred$fit-1.96*(pred$se.fit))
ca_cases$more_ub_50_65 <- exp(pred$fit+1.96*(pred$se.fit))

model3 <- glm(`[50,65)`~log(less_inf_0_12), data=ca_cases %>% filter(weeks_since_Jan2020 < vaccine), "quasipoisson")
pred <- predict(model3, newdata = ca_cases %>% select(less_inf_0_12), se.fit = T)
ca_cases$less_pred_50_65 <- exp(pred$fit)
ca_cases$less_lb_50_65 <- exp(pred$fit-1.96*(pred$se.fit))
ca_cases$less_ub_50_65 <- exp(pred$fit+1.96*(pred$se.fit))

# 65+ cases averted
model4 <- glm(`[65,Inf)`~log(more_inf_0_12), data=ca_cases %>% filter(weeks_since_Jan2020 < vaccine), "quasipoisson")
pred <- predict(model4, newdata = ca_cases %>% select(more_inf_0_12), se.fit = T)
ca_cases$more_pred_65 <- exp(pred$fit)
ca_cases$more_lb_65 <- exp(pred$fit-1.96*pred$se.fit)
ca_cases$more_ub_65 <- exp(pred$fit+1.96*pred$se.fit)

model4 <- glm(`[65,Inf)`~log(less_inf_0_12), data=ca_cases %>% filter(weeks_since_Jan2020 < vaccine), "quasipoisson")
pred <- predict(model4, newdata = ca_cases %>% select(less_inf_0_12), se.fit = T)
ca_cases$less_pred_65 <- exp(pred$fit)
ca_cases$less_lb_65 <- exp(pred$fit-1.96*pred$se.fit)
ca_cases$less_ub_65 <- exp(pred$fit+1.96*pred$se.fit)



vaccine <- 48
### cases averted beginning phase 1a of vaccination
post_vaccine <- ca_cases %>% filter(weeks_since_Jan2020 >= vaccine)

post_vaccine <- post_vaccine %>% mutate(more_pred_12_18 = ifelse(more_pred_12_18 >= `[12,18)`, more_pred_12_18, `[12,18)`),
                                        more_lb_12_18 = ifelse(more_lb_12_18 >= `[12,18)`, more_lb_12_18, `[12,18)`),
                                        more_ub_12_18 = ifelse(more_ub_12_18 >= `[12,18)`, more_ub_12_18, `[12,18)`),
                                        less_pred_12_18 = ifelse(less_pred_12_18 >= `[12,18)`, less_pred_12_18, `[12,18)`),
                                        less_lb_12_18 = ifelse(less_lb_12_18 >= `[12,18)`, less_lb_12_18, `[12,18)`),
                                        less_ub_12_18 = ifelse(less_ub_12_18 >= `[12,18)`, less_ub_12_18, `[12,18)`))
post_vaccine <- post_vaccine %>% mutate(more_pred_18_50 = ifelse(more_pred_18_50 >= `[18,50)`, more_pred_18_50, `[18,50)`),
                                        more_lb_18_50 = ifelse(more_lb_18_50 >= `[18,50)`, more_lb_18_50, `[18,50)`),
                                        more_ub_18_50 = ifelse(more_ub_18_50 >= `[18,50)`, more_ub_18_50, `[18,50)`),
                                        less_pred_18_50 = ifelse(less_pred_18_50 >= `[18,50)`, less_pred_18_50, `[18,50)`),
                                        less_lb_18_50 = ifelse(less_lb_18_50 >= `[18,50)`, less_lb_18_50, `[18,50)`),
                                        less_ub_18_50 = ifelse(less_ub_18_50 >= `[18,50)`, less_ub_18_50, `[18,50)`))
post_vaccine <- post_vaccine %>% mutate(more_pred_50_65 = ifelse(more_pred_50_65 >= `[50,65)`, more_pred_50_65, `[50,65)`),
                                        more_lb_50_65 = ifelse(more_lb_50_65 >= `[50,65)`, more_lb_50_65, `[50,65)`),
                                        more_ub_50_65 = ifelse(more_ub_50_65 >= `[50,65)`, more_ub_50_65, `[50,65)`),
                                        less_pred_50_65 = ifelse(less_pred_50_65 >= `[50,65)`, less_pred_50_65, `[50,65)`),
                                        less_lb_50_65 = ifelse(less_lb_50_65 >= `[50,65)`, less_lb_50_65, `[50,65)`),
                                        less_ub_50_65 = ifelse(less_ub_50_65 >= `[50,65)`, less_ub_50_65, `[50,65)`))
post_vaccine <- post_vaccine %>% mutate(more_pred_65 = ifelse(more_pred_65 >= `[65,Inf)`, more_pred_65, `[65,Inf)`),
                                        more_lb_65 = ifelse(more_lb_65 >= `[65,Inf)`, more_lb_65, `[65,Inf)`),
                                        more_ub_65 = ifelse(more_ub_65 >= `[65,Inf)`, more_ub_65, `[65,Inf)`),
                                        less_pred_65 = ifelse(less_pred_65 >= `[65,Inf)`, less_pred_65, `[65,Inf)`),
                                        less_lb_65 = ifelse(less_lb_65 >= `[65,Inf)`, less_lb_65, `[65,Inf)`),
                                        less_ub_65 = ifelse(less_ub_65 >= `[65,Inf)`, less_ub_65, `[65,Inf)`))

post_vaccine <- post_vaccine %>% mutate(total_cases = rowSums(post_vaccine %>% select("[12,18)","[18,50)","[50,65)","[65,Inf)")),
                                        more_total_cases_pred = rowSums(post_vaccine %>% select(more_pred_12_18, more_pred_18_50, more_pred_50_65, more_pred_65)),
                                        more_total_cases_lb = rowSums(post_vaccine %>% select(more_lb_12_18, more_lb_18_50, more_lb_50_65, more_lb_65)),
                                        more_total_cases_ub = rowSums(post_vaccine %>% select(more_ub_12_18, more_ub_18_50, more_ub_50_65, more_ub_65)),
                                        less_total_cases_pred = rowSums(post_vaccine %>% select(less_pred_12_18, less_pred_18_50, less_pred_50_65, less_pred_65)),
                                        less_total_cases_lb = rowSums(post_vaccine %>% select(less_lb_12_18, less_lb_18_50, less_lb_50_65, less_lb_65)),
                                        less_total_cases_ub = rowSums(post_vaccine %>% select(less_ub_12_18, less_ub_18_50, less_ub_50_65, less_ub_65)))

saveRDS(post_vaccine %>% select(!c(color)), "results/primary/primary-model-test-delta.RDS")
