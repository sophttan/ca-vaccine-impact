###################################################################################################
#Title: Hospitalization and deaths from primary model of averted cases
# Sensitivity analysis testing different levels of infectiousness of the delta variant
#Author: Sophia Tan
###################################################################################################

rm(list=ls())
source("configuration.R")

res <- readRDS("results/primary/primary-model-test-delta.RDS")
rates <- readRDS("data/hosp_death_rate_week_month_rates.RDS") %>% filter(weeks_since_Jan2020 >= 48)


total_res_hosp <- NULL
total_res_death <- NULL


prep_res <- function(data, age) {
  res <- data[,1:6]-data[,7]

  res <- as.data.frame(cbind(res, data))
  names(res) <- c("pred_averted_more", "lb_averted_more", "ub_averted_more",
                  "pred_averted_less", "lb_averted_less", "ub_averted_less",
                  "pred_more", "lb_more", "ub_more",
                  "pred_less", "lb_less", "ub_less", "actual")

  res$age <- age

  res <- res %>% mutate(pred_averted_more = ifelse(pred_averted_more < 0, 0, pred_averted_more),
                        lb_averted_more = ifelse(lb_averted_more < 0, 0, lb_averted_more),
                        ub_averted_more = ifelse(ub_averted_more < 0, 0, ub_averted_more),
                        pred_more = ifelse(pred_more < actual, actual, pred_more),
                        lb_more = ifelse(lb_more < actual, actual, lb_more),
                        ub_more = ifelse(ub_more < actual, actual, ub_more),
                        pred_averted_less = ifelse(pred_averted_less < 0, 0, pred_averted_less),
                        lb_averted_less = ifelse(lb_averted_less < 0, 0, lb_averted_less),
                        ub_averted_less = ifelse(ub_averted_less < 0, 0, ub_averted_less),
                        pred_less = ifelse(pred_less < actual, actual, pred_more),
                        lb_less = ifelse(lb_less < actual, actual, lb_less),
                        ub_less = ifelse(ub_less < actual, actual, ub_less))

  res
}

groups <- (rates$age_hand_cut %>% unique())[3:5]
for (group in 1:3) {
  subset <- rates %>% filter(age_hand_cut==groups[group])
  if (group==1){
    subset_cases <- res %>% select(more_pred_18_50, more_lb_18_50, more_ub_18_50,
                                   less_pred_18_50, less_lb_18_50, less_ub_18_50)
  } else if (group==2) {
    subset_cases <- res %>% select(more_pred_50_65, more_lb_50_65, more_ub_50_65,
                                   less_pred_50_65, less_lb_50_65, less_ub_50_65)
  } else{
    subset_cases <- res %>% select(more_pred_65, more_lb_65, more_ub_65,
                                   less_pred_65, less_lb_65, less_ub_65)
  }


  hosp <- subset_cases * subset$hosp_rate/100
  death <- subset_cases * subset$death_rate/100

  hosp_res <- prep_res(cbind(hosp,subset$num_hosp), groups[group])
  death_res <- prep_res(cbind(death,subset$num_died), groups[group])

  total_res_hosp <- rbind(total_res_hosp, hosp_res)
  total_res_death <- rbind(total_res_death, death_res)

}


saveRDS(total_res_death, "results/hospitalizations and deaths/death_results_primary_model_test_delta.RDS")
saveRDS(total_res_hosp, "results/hospitalizations and deaths/hosp_results_primary_model_test_delta.RDS")


