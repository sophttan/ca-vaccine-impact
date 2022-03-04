###################################################################################################
#Title: Functions for hospitalizations and deaths
#Author: Sophia Tan
###################################################################################################


prep_res <- function(data, age) {
  res <- data[,2:4]-data[,1]

  res <- as.data.frame(cbind(res, data))
  names(res) <- c("pred_averted", "lb_averted", "ub_averted",
                  "actual_hosp", "pred_hosp", "lb_hosp", "ub_hosp")

  res$age <- age

  res <- res %>% mutate(pred_averted = ifelse(pred_averted < 0, 0, pred_averted),
                        lb_averted = ifelse(lb_averted < 0, 0, lb_averted),
                        ub_averted = ifelse(ub_averted < 0, 0, ub_averted),
                        pred_hosp = ifelse(pred_hosp < actual_hosp, actual_hosp, pred_hosp),
                        lb_hosp = ifelse(lb_hosp < actual_hosp, actual_hosp, lb_hosp),
                        ub_hosp = ifelse(ub_hosp < actual_hosp, actual_hosp, ub_hosp))

  res
}

format_res <- function(v) {
  paste0(v[1], " (", v[2], ", ", v[3], ")")
}
