###################################################################################################
#Title: Functions for plotting results in CA using susceptibility-based model
#Author: Sophia Tan
###################################################################################################

setwd("/mnt/projects/covid_partners/ucsf_lo")

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


# plot differences between predicted cases in unvacccinated and vaccinated scenarios
# takes in data frame with mean and confidence intervals for predicted cases over time
plot_case_predictions <- function(data) {
  data %>% ggplot(aes(weeks_since_Jan2020)) +
    geom_ribbon(aes(ymax=ub, ymin=lb), fill="grey80") +
    geom_line(aes(y=obs, color="Observed cases")) + 
    geom_line(aes(y=mean, color="Predicted cases")) +
    ylab("Weekly no. cases") + 
    scale_x_continuous(name = "Month", breaks=dates2$weeks, labels=dates2$month) + 
    scale_y_continuous(labels = comma) + 
    theme(legend.title=element_blank(), axis.text.x = element_text(angle=90), 
          axis.line = element_line(colour = "black"),
          panel.grid.minor = element_blank())
}

# plot relative reduction in cases
# takes in data frame with mean and confidence intervals for predicted cases over time
plot_relative_reduction <- function(data) {
  data %>%
    ggplot(aes(x=weeks_since_Jan2020)) +
    geom_ribbon(aes(ymax=(ub-obs)/ub*100, 
                    ymin=(lb-obs)/lb*100), fill="grey80") +
    geom_line(aes(y=(mean-obs)/mean*100, color="% reduction cases")) +
    geom_line(aes(y=vacc, color="% cumulative vaccination")) +
    scale_y_continuous(name = "Vaccine coverage or case reduction (%)", labels = comma) +
    scale_x_continuous(name = "Month", breaks=dates2$weeks, labels=dates2$month) +
    theme(legend.title=element_blank(), axis.text.x = element_text(angle=90),
          axis.line = element_line(colour = "black"),
          panel.grid.minor = element_blank())
}