###################################################################################################
#Title: Functions for hospitalizations and deaths
#Author: Sophia Tan
###################################################################################################


prep_res <- function(data, age) {
  res <- data[,1:3]-data[,4]

  res <- as.data.frame(cbind(res, data))
  names(res) <- c("pred_averted", "lb_averted", "ub_averted",
                  "pred", "lb", "ub", "obs")

  res$age <- age

  res <- res %>% mutate(pred_averted = ifelse(pred_averted < 0, 0, pred_averted),
                        lb_averted = ifelse(lb_averted < 0, 0, lb_averted),
                        ub_averted = ifelse(ub_averted < 0, 0, ub_averted),
                        pred = ifelse(pred < obs, obs, pred),
                        lb = ifelse(lb < obs, obs, lb),
                        ub = ifelse(ub < obs, obs, ub))

  res
}

format_res <- function(v) {
  paste0(v[1], " (", v[2], ", ", v[3], ")")
}
