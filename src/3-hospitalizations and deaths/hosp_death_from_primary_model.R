###################################################################################################
#Title: Hospitalization and deaths from primary model of averted cases
#Author: Sophia Tan
###################################################################################################

rm(list=ls())
source("configuration.R")
primary_cases <- readRDS("results/primary/primary-model-main-results.RDS")
rates <- readRDS("data/hosp_death_rate_week_month_rates.RDS") %>% filter(weeks_since_Jan2020 >= 48)


total_res_hosp <- NULL
total_res_death <- NULL

prep_res <- function(data, age) {
  res <- data[,2:4]-data[,1]

  res <- as.data.frame(cbind(res, data))
  names(res) <- c("pred_averted", "lb_averted", "ub_averted",
                  "actual_hosp", "pred_hosp", "lb_hosp", "ub_hosp")

  res$weeks_since_Jan2020 <- 48:93
  res$age <- age

  res <- res %>% mutate(pred_averted = ifelse(pred_averted < 0, 0, pred_averted),
                        lb_averted = ifelse(lb_averted < 0, 0, lb_averted),
                        ub_averted = ifelse(ub_averted < 0, 0, ub_averted),
                        pred_hosp = ifelse(pred_hosp < actual_hosp, actual_hosp, pred_hosp),
                        lb_hosp = ifelse(lb_hosp < actual_hosp, actual_hosp, lb_hosp),
                        ub_hosp = ifelse(ub_hosp < actual_hosp, actual_hosp, ub_hosp))

  res
}

groups <- (rates$age_hand_cut %>% unique())[3:5]
for (group in 1:3) {
  subset <- rates %>% filter(age_hand_cut==groups[group])
  if (group==1){
    subset_cases <- primary_cases %>% select(pred_18_50, lb_18_50, ub_18_50)
  } else if (group==2){
    subset_cases <- primary_cases %>% select( pred_50_65, lb_50_65, ub_50_65)
  } else{
    subset_cases <- primary_cases %>% select(pred_65, lb_65, ub_65)
  }

  hosp <- subset_cases * subset$hosp_rate/100
  death <- subset_cases * subset$death_rate/100

  hosp_res <- prep_res(cbind(subset$num_hosp, hosp), groups[group])
  death_res <- prep_res(cbind(subset$num_died, death), groups[group])

  total_res_hosp <- rbind(total_res_hosp, hosp_res)
  total_res_death <- rbind(total_res_death, death_res)
}

saveRDS(total_res_death, "results/hospitalizations and deaths/death_results_primary_model.RDS")
saveRDS(total_res_hosp, "results/hospitalizations and deaths/hosp_results_primary_model.RDS")

