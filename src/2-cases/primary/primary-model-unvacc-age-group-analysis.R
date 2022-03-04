###################################################################################################
#Title: Direct Effects of Vaccination in CA Primary Analysis
# Sensitivity analysis varying time definition of beginning of vaccine era
#Author: Sophia Tan
###################################################################################################

rm(list=ls())
source("configuration.R")
source("figure scripts/plot-functions.R")

cases <- read_csv("/mnt/projects/covid_partners/ucsf_lo/Direct Effects Analysis/Data/ca_case_data.csv")

ca_cases <- cases %>% mutate(`[0,18)`=`[0,12)`+`[12,18)`)

# load in dataset mapping weeks since January 1, 2020 to months since January 1, 2020
dates2 <- read_csv("data/weeks_months_data.csv")

# load in vaccination data
vacc <- readRDS("data/vaccination_coverage_data.RDS")

beginning <- 22
vaccine <- 48


# use <18 population as unvaccination population
# visually check relationships between weekly cases unvaccinated group and other age groups
calibration <- ca_cases %>% filter(weeks_since_Jan2020 >= beginning & weeks_since_Jan2020 < vaccine)
calibration %>%
  ggplot(aes(`[0,18)`, `[18,50)`)) + geom_point()
calibration %>%
  ggplot(aes(`[0,18)`, `[50,65)`)) + geom_point()
calibration %>%
  ggplot(aes(`[0,18)`, `[65,Inf)`)) + geom_point()


# 18-50 cases averted
model <- glm(`[18,50)`~log(`[0,18)`),
             data=ca_cases %>% filter(weeks_since_Jan2020 >= beginning & weeks_since_Jan2020 < vaccine),
             "quasipoisson")
pred <- predict(model, newdata = ca_cases %>% select(`[0,18)`), se.fit = T)
ca_cases$pred_18_50 <- exp(pred$fit)
ca_cases$lb_18_50 <- exp(pred$fit-1.96*(pred$se.fit))
ca_cases$ub_18_50 <- exp(pred$fit+1.96*(pred$se.fit))

# 50-65 cases averted
model <- glm(`[50,65)`~log(`[0,18)`),
             data=ca_cases %>% filter(weeks_since_Jan2020 >= beginning & weeks_since_Jan2020 < vaccine),
             "quasipoisson")
pred <- predict(model, newdata = ca_cases %>% select(`[0,18)`), se.fit = T)
ca_cases$pred_50_65 <- exp(pred$fit)
ca_cases$lb_50_65 <- exp(pred$fit-1.96*(pred$se.fit))
ca_cases$ub_50_65 <- exp(pred$fit+1.96*(pred$se.fit))

# 65+ cases averted
model <- glm(`[65,Inf)`~log(`[0,18)`),
             data=ca_cases %>% filter(weeks_since_Jan2020 >= beginning & weeks_since_Jan2020 < vaccine),
             "quasipoisson")
pred <- predict(model, newdata = ca_cases %>% select(`[0,18)`), se.fit = T)
ca_cases$pred_65 <- exp(pred$fit)
ca_cases$lb_65 <- exp(pred$fit-1.96*pred$se.fit)
ca_cases$ub_65 <- exp(pred$fit+1.96*pred$se.fit)

post_vaccine <- filter(ca_cases, weeks_since_Jan2020 >= vaccine)
post_vaccine <- post_vaccine %>% mutate(pred_18_50 = ifelse(pred_18_50 >= `[18,50)`, pred_18_50, `[18,50)`),
                                        lb_18_50 = ifelse(lb_18_50 >= `[18,50)`, lb_18_50, `[18,50)`),
                                        ub_18_50 = ifelse(ub_18_50 >= `[18,50)`, ub_18_50, `[18,50)`))
post_vaccine <- post_vaccine %>% mutate(pred_50_65 = ifelse(pred_50_65 >= `[50,65)`, pred_50_65, `[50,65)`),
                                        lb_50_65 = ifelse(lb_50_65 >= `[50,65)`, lb_50_65, `[50,65)`),
                                        ub_50_65 = ifelse(ub_50_65 >= `[50,65)`, ub_50_65, `[50,65)`))
post_vaccine <- post_vaccine %>% mutate(pred_65 = ifelse(pred_65 >= `[65,Inf)`, pred_65, `[65,Inf)`),
                                        lb_65 = ifelse(lb_65 >= `[65,Inf)`, lb_65, `[65,Inf)`),
                                        ub_65 = ifelse(ub_65 >= `[65,Inf)`, ub_65, `[65,Inf)`))
# total cases 12+
post_vaccine <- post_vaccine %>% mutate(total_cases = rowSums(post_vaccine %>% select("[18,50)","[50,65)","[65,Inf)")),
                                        total_cases_pred = rowSums(post_vaccine %>% select(pred_18_50, pred_50_65, pred_65)),
                                        total_cases_lb = rowSums(post_vaccine %>% select(lb_18_50, lb_50_65, lb_65)),
                                        total_cases_ub = rowSums(post_vaccine %>% select(ub_18_50, ub_50_65, ub_65)))

post_vaccine <- post_vaccine %>% mutate(unvacc = "[0,18)")





# use 12-17 population as unvaccination population
# visually check relationships between weekly cases unvaccinated group and other age groups
calibration <- ca_cases %>% filter(weeks_since_Jan2020 >= beginning & weeks_since_Jan2020 < vaccine)
calibration %>%
  ggplot(aes(`[12,18)`, `[18,50)`)) + geom_point()
calibration %>%
  ggplot(aes(`[12,18)`, `[50,65)`)) + geom_point()
calibration %>%
  ggplot(aes(`[12,18)`, `[65,Inf)`)) + geom_point()


# 18-50 cases averted
model <- glm(`[18,50)`~log(`[12,18)`),
             data=ca_cases %>% filter(weeks_since_Jan2020 >= beginning & weeks_since_Jan2020 < vaccine),
             "quasipoisson")
pred <- predict(model, newdata = ca_cases %>% select(`[12,18)`), se.fit = T)
ca_cases$pred_18_50 <- exp(pred$fit)
ca_cases$lb_18_50 <- exp(pred$fit-1.96*(pred$se.fit))
ca_cases$ub_18_50 <- exp(pred$fit+1.96*(pred$se.fit))

# 50-65 cases averted
model <- glm(`[50,65)`~log(`[12,18)`),
             data=ca_cases %>% filter(weeks_since_Jan2020 >= beginning & weeks_since_Jan2020 < vaccine),
             "quasipoisson")
pred <- predict(model, newdata = ca_cases %>% select(`[12,18)`), se.fit = T)
ca_cases$pred_50_65 <- exp(pred$fit)
ca_cases$lb_50_65 <- exp(pred$fit-1.96*(pred$se.fit))
ca_cases$ub_50_65 <- exp(pred$fit+1.96*(pred$se.fit))

# 65+ cases averted
model <- glm(`[65,Inf)`~log(`[12,18)`),
             data=ca_cases %>% filter(weeks_since_Jan2020 >= beginning & weeks_since_Jan2020 < vaccine),
             "quasipoisson")
pred <- predict(model, newdata = ca_cases %>% select(`[12,18)`), se.fit = T)
ca_cases$pred_65 <- exp(pred$fit)
ca_cases$lb_65 <- exp(pred$fit-1.96*pred$se.fit)
ca_cases$ub_65 <- exp(pred$fit+1.96*pred$se.fit)

post_vaccine1 <- filter(ca_cases, weeks_since_Jan2020 >= vaccine)
post_vaccine1 <- post_vaccine1 %>% mutate(pred_18_50 = ifelse(pred_18_50 >= `[18,50)`, pred_18_50, `[18,50)`),
                                        lb_18_50 = ifelse(lb_18_50 >= `[18,50)`, lb_18_50, `[18,50)`),
                                        ub_18_50 = ifelse(ub_18_50 >= `[18,50)`, ub_18_50, `[18,50)`))
post_vaccine1 <- post_vaccine1 %>% mutate(pred_50_65 = ifelse(pred_50_65 >= `[50,65)`, pred_50_65, `[50,65)`),
                                        lb_50_65 = ifelse(lb_50_65 >= `[50,65)`, lb_50_65, `[50,65)`),
                                        ub_50_65 = ifelse(ub_50_65 >= `[50,65)`, ub_50_65, `[50,65)`))
post_vaccine1 <- post_vaccine1 %>% mutate(pred_65 = ifelse(pred_65 >= `[65,Inf)`, pred_65, `[65,Inf)`),
                                        lb_65 = ifelse(lb_65 >= `[65,Inf)`, lb_65, `[65,Inf)`),
                                        ub_65 = ifelse(ub_65 >= `[65,Inf)`, ub_65, `[65,Inf)`))
# total cases 12+
post_vaccine1 <- post_vaccine1 %>% mutate(total_cases = rowSums(post_vaccine %>% select("[18,50)","[50,65)","[65,Inf)")),
                                        total_cases_pred = rowSums(post_vaccine %>% select(pred_18_50, pred_50_65, pred_65)),
                                        total_cases_lb = rowSums(post_vaccine %>% select(lb_18_50, lb_50_65, lb_65)),
                                        total_cases_ub = rowSums(post_vaccine %>% select(ub_18_50, ub_50_65, ub_65)))

post_vaccine1 <- post_vaccine1 %>% mutate(unvacc = "[12,18)")



rbind(post_vaccine, post_vaccine1) %>% saveRDS("results/primary/primary-model-unvacc-age-group.RDS")
