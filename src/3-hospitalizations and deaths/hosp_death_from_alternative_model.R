###################################################################################################
#Title: Hospitalization and deaths from alternative model of averted cases
#Author: Sophia Tan
###################################################################################################

rm(list=ls())
source("configuration.R")

dates <- read_csv("data/weeks_months_data.csv")

hosp_data <- readRDS("data/hosp_death_rate_week_month_rates.RDS") %>% filter(weeks_since_Jan2020 >=48)
res_12_18 <- readRDS("results/alternative/main/res_12_18_sim.RDS") %>% filter(weeks_since_Jan2020>=48)
res_18_50 <- readRDS("results/alternative/main/res_18_50_sim.RDS") %>% filter(weeks_since_Jan2020>=48)
res_50_65 <- readRDS("results/alternative/main/res_50_65_sim.RDS") %>% filter(weeks_since_Jan2020>=48)
res_65 <- readRDS("results/alternative/main/res_65_sim.RDS") %>% filter(weeks_since_Jan2020>=48)

prep_res <- function(data, age) {
  res <- data[,1:3]-data[,4]

  res <- as.data.frame(cbind(res, data))
  names(res) <- c("pred_averted", "lb_averted", "ub_averted",
                  "pred", "lb", "ub", "actual_hosp")

  res$weeks_since_Jan2020 <- 48:93
  res$age <- age

  res <- res %>% mutate(pred_averted = ifelse(pred_averted < 0, 0, pred_averted),
                        lb_averted = ifelse(lb_averted < 0, 0, lb_averted),
                        ub_averted = ifelse(ub_averted < 0, 0, ub_averted),
                        pred = ifelse(pred < actual_hosp, actual_hosp, pred),
                        lb = ifelse(lb < actual_hosp, actual_hosp, lb),
                        ub = ifelse(ub < actual_hosp, actual_hosp, ub))

  res
}

groups<-hosp_data[1:5,]$age_hand_cut[3:5]
results <- c("res_18_50", "res_50_65", "res_65")
total_res_hosp <- NULL
total_res_death <- NULL

for (i in 1:3) {
  res <- get(results[i])
  group <- groups[i]

  subset <- hosp_data %>% filter(age_hand_cut==group)

  hosp <- (subset$hosp_rate/100) * res[,2:4]
  death <- (subset$death_rate/100) * res[,2:4]
  hosp$obs <- subset$num_hosp
  death$obs <- subset$num_died

  hosp_res <- prep_res(hosp, group)
  death_res <- prep_res(death, group)

  total_res_hosp <- rbind(total_res_hosp, hosp_res)
  total_res_death <- rbind(total_res_death, death_res)

}


saveRDS(total_res_death, "results/hospitalizations and deaths/death_results_alternative_model.RDS")
saveRDS(total_res_hosp, "results/hospitalizations and deaths/hosp_results_alternative_model.RDS")


