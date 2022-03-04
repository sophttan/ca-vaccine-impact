###################################################################################################
#Title: Direct Effects of Vaccination in CA Primary Analysis
# Sensitivity analysis varying time definition of beginning of vaccine era
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

res <- NULL

beginning <- 22
ca_cases <- cases  %>% left_join(vacc, "weeks_since_Jan2020") %>%
  replace_na(list(vacc_cum_12_18 = 0, vacc_cum_18_50=0, vacc_cum_50_65=0, vacc_cum_65=0, vacc_cum=0))
ca_cases <- ca_cases %>% filter(weeks_since_Jan2020 >= beginning)



for (vaccine in seq(48, round(48*1.1))) {
  model1 <- glm(`[12,18)`~log(`[0,12)`),
                data=ca_cases %>% filter(weeks_since_Jan2020 >= beginning & weeks_since_Jan2020 < vaccine),
                "quasipoisson")
  pred <- predict(model1, newdata = ca_cases %>% select(`[0,12)`), se.fit = T)
  ca_cases$pred_12_18 <- exp(pred$fit)
  ca_cases$lb_12_18 <- exp(pred$fit-1.96*(pred$se.fit))
  ca_cases$ub_12_18 <- exp(pred$fit+1.96*(pred$se.fit))

  model2 <- glm(`[18,50)`~log(`[0,12)`),
                data=ca_cases %>% filter(weeks_since_Jan2020 >= beginning & weeks_since_Jan2020 < vaccine),
                "quasipoisson")
  pred <- predict(model2, newdata = ca_cases %>% select(`[0,12)`), se.fit = T)
  ca_cases$pred_18_50 <- exp(pred$fit)
  ca_cases$lb_18_50 <- exp(pred$fit-1.96*(pred$se.fit))
  ca_cases$ub_18_50 <- exp(pred$fit+1.96*(pred$se.fit))

  model3 <- glm(`[50,65)`~log(`[0,12)`),
                data=ca_cases %>% filter(weeks_since_Jan2020 >= beginning & weeks_since_Jan2020 < vaccine),
                "quasipoisson")
  pred <- predict(model3, newdata = ca_cases %>% select(`[0,12)`), se.fit = T)
  ca_cases$pred_50_65 <- exp(pred$fit)
  ca_cases$lb_50_65 <- exp(pred$fit-1.96*(pred$se.fit))
  ca_cases$ub_50_65 <- exp(pred$fit+1.96*(pred$se.fit))

  model4 <- glm(`[65,Inf)`~log(`[0,12)`),
                data=ca_cases %>% filter(weeks_since_Jan2020 >= beginning & weeks_since_Jan2020 < vaccine),
                "quasipoisson")
  pred <- predict(model4, newdata = ca_cases %>% select(`[0,12)`), se.fit = T)
  ca_cases$pred_65 <- exp(pred$fit)
  ca_cases$lb_65 <- exp(pred$fit-1.96*pred$se.fit)
  ca_cases$ub_65 <- exp(pred$fit+1.96*pred$se.fit)

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

  res <- rbind(res, cbind(post_vaccine, vaccine))
}


saveRDS(res, "results/primary/primary-model-vacc-threshold.RDS")
