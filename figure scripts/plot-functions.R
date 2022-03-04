###################################################################################################
#Title: Direct Effects of Vaccination in CA
# Functions for figures
#Author: Sophia Tan
###################################################################################################

##### Functions for primary model

### Functions for making plots
plot_averted_cases <- function(data, age_group) {
  age_group_pred <- gsub(",", "_", str_extract(age_group, "[0-9]+,[0-9]+"))

  if(grepl("Inf", age_group)) {
    age_group_pred <- str_extract(age_group, "[0-9]+")
  }

  data %>%
    ggplot(aes(x=weeks_since_Jan2020)) +
    geom_ribbon(aes(ymax=.data[[paste0("ub_", age_group_pred)]], ymin=.data[[paste0("lb_", age_group_pred)]]), fill="grey80") +
    geom_line(aes(y=.data[[age_group]], color="Observed outcome")) +
    geom_line(aes(y=.data[[paste0("pred_", age_group_pred)]], color="Predicted outcome")) +
    geom_vline(xintercept = 74, size=.3, lty="longdash") +
    scale_y_continuous(name="Weekly outcome", labels=comma) +
    scale_x_continuous(name = "Month", breaks=dates2$weeks, labels=dates2$month) +
    theme(legend.title=element_blank(), axis.text.x = element_text(angle=90),
          axis.line = element_line(colour = "black"),
          panel.grid.minor = element_blank())
}

plot_model_fit <- function(data, model, age_1, age_2, age_1_label, age_2_label){
  d <- data.frame(age_1 = 1000:8000)%>% rename("[0,12)"=age_1)
  ggplot(data, aes(.data[[age_1]], .data[[age_2]])) + geom_point() +
    geom_line(data = d, aes(x=.data[[age_1]], y=exp(predict(model, newdata = d))), size=.2) +
    scale_x_continuous(name=paste("Weekly outcome", age_1_label, "years"), label=comma, limits = c(min(data[[age_1]]), 8000)) +
    scale_y_continuous(name=paste("Weekly outcome", age_2_label, "years"), label=comma) +
    theme(legend.title = element_blank())
}



##### Functions for alternative model

# plot differences between predicted cases in unvacccinated and vaccinated scenarios
# takes in data frame with mean and confidence intervals for predicted cases over time
plot_case_predictions <- function(data) {
  data %>% ggplot(aes(weeks_since_Jan2020)) +
    geom_ribbon(aes(ymax=ub, ymin=lb), fill="grey80") +
    geom_line(aes(y=obs, color="Observed outcome")) +
    geom_line(aes(y=mean, color="Predicted outcome")) +
    ylab("Weekly cases") +
    scale_x_continuous(name = "Month", breaks=dates2$weeks, labels=dates2$month) +
    theme(legend.title=element_blank(), axis.text.x = element_text(angle=90),
          axis.line = element_line(colour = "black"),
          panel.grid.minor = element_blank())
}


plot_hosp_death <- function(d, a, vacc, max, title) {
  if ("pred_hosp" %in% names(d)){
    d <- d %>% filter(age==a) %>% rename("obs"="actual_hosp", "mean"="pred_hosp", "ub"="ub_hosp", "lb"="lb_hosp")
  } else {
    d <- d %>% filter(age==a) %>% rename("obs"="actual_hosp", "mean"="pred")
  }
  d$vacc <- vacc
  plot_case_predictions(d) +
    geom_line(aes(y=vacc*(max/100), color="% vaccination")) +
    geom_vline(xintercept = 74, size=.3, lty="longdash")+
    scale_y_continuous(name=title,
                       labels=comma, expand = expansion(mult = c(0, .05)),
                       limits = c(0, max*1.01),
                       # Add a second axis and specify its features
                       sec.axis = sec_axis(~./(max/100), breaks=seq(0, 100, 25), name="Cumulative % vaccination")
    )
}
