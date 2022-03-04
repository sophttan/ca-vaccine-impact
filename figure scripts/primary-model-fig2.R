###################################################################################################
#Title: Plot of overall primary model results - cases, hospitalizations, and deaths (Figure 2)
#Author: Sophia Tan
###################################################################################################

rm(list=ls())
source("configuration.R")
primary_cases <- readRDS("results/primary/primary-model-main-results.RDS")
death <- readRDS("results/hospitalizations and deaths/death_results_primary_model.RDS")
hosp <- readRDS("results/hospitalizations and deaths/hosp_results_primary_model.RDS")

dates <- read_csv("data/weeks_months_data.csv")

hosp <- hosp %>% filter(age!="[12,18)") %>% select(!age) %>% group_by(weeks_since_Jan2020) %>% summarise_all(sum) %>%
  mutate(vacc_cum = primary_cases$vacc_cum_18up)
scalar2 <- max(hosp$ub_hosp)/100
plot2 <- hosp %>%
  ggplot(aes(weeks_since_Jan2020, pred_hosp)) +
  geom_ribbon(aes(ymax=ub_hosp, ymin=lb_hosp), fill="grey80") +
  geom_line(aes(color="Predicted outcome")) + geom_line(aes(y=actual_hosp, color="Observed outcome")) +
  geom_line(aes(y=vacc_cum*scalar2, color="Cumulative vaccination (%)")) +
  scale_y_continuous(labels=comma,
                     expand = expansion(mult=c(0, 0.05)),
                     sec.axis = sec_axis(~./scalar2, breaks=seq(0,100,25))) +
  scale_x_continuous(name = "Month", breaks=dates$weeks, labels=dates$month) +
  geom_vline(aes(xintercept=74), linetype="dashed") +
  labs(title="B", subtitle = "Hospitalizations") +
  theme(legend.title=element_blank(), axis.text.x = element_text(angle=90),
        axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(), plot.subtitle = element_text(hjust = 0),
        axis.title.y.left = element_blank())
plot2

scalar3 <- scalar <- max(death$ub_hosp)/100
plot3 <- death %>% filter(age!="[12,18)") %>% select(!age) %>% group_by(weeks_since_Jan2020) %>% summarise_all(sum) %>%
  mutate(vacc_cum = primary_cases$vacc_cum_18up) %>%
  ggplot(aes(weeks_since_Jan2020, pred_hosp)) +
  geom_ribbon(aes(ymax=ub_hosp, ymin=lb_hosp), fill="grey80") +
  geom_line(aes(color="Predicted outcome")) + geom_line(aes(y=actual_hosp, color="Observed outcome")) +
  geom_line(aes(y=vacc_cum*scalar3, color="Cumulative vaccination (%)")) +
  scale_y_continuous(labels=comma,
                     expand = expansion(mult=c(0, 0.05)),
                     sec.axis = sec_axis(~./scalar3, breaks=seq(0,100,25), name="Cumulative vaccine coverage (%)")) +
  geom_vline(aes(xintercept=74), linetype="dashed") +
  labs(title="C", subtitle = "Deaths") +
  scale_x_continuous(name = "Month", breaks=dates$weeks, labels=dates$month) +
  theme(legend.title=element_blank(), axis.text.x = element_text(angle=90),
        axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(), plot.subtitle = element_text(hjust=0),
        axis.title.y.left = element_blank())
plot3

scalar1 <- max(primary_cases$total_cases_ub)/100
plot1 <- primary_cases %>%
  ggplot(aes(x=weeks_since_Jan2020)) +
  geom_ribbon(aes(ymax=total_cases_ub, ymin=total_cases_lb), fill="grey80") +
  geom_line(aes(y=total_cases, color="Observed outcome")) +
  geom_line(aes(y=total_cases_pred, color="Predicted outcome")) +
  geom_line(aes(y=vacc_cum*scalar1, color="Cumulative vaccination (%)")) +
  scale_y_continuous(name="Weekly outcome", labels=comma,
                     expand = expansion(mult=c(0, 0.05)),
                     sec.axis = sec_axis(~./scalar1, breaks=seq(0,100,25))) +
  geom_vline(aes(xintercept=74), linetype="dashed") +
  scale_x_continuous(name = "Month", breaks=dates$weeks, labels=dates$month) +
  labs(title="A", subtitle = "Cases") +
  theme(legend.title=element_blank(), axis.text.x = element_text(angle=90),
        axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(), plot.subtitle = element_text(hjust=0))
plot1

library(devEMF)
emf(file="figures/all_primary_model_results-fig2.emf", width = 10, height=4)
(plot1+plot2+plot3+plot_layout(guides = "collect", nrow=1) & theme(legend.position = "bottom"))
dev.off()


(plot1+plot2+plot3+plot_layout(guides = "collect", nrow=1) & theme(legend.position = "bottom")) %>%
  ggsave(filename="figures/all_primary_model_results-fig2.jpg", width = 10, height=4)
