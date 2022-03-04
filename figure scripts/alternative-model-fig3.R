###################################################################################################
#Title: Plot of overall alternative model results - cases, hospitalizations, and deaths (Figure 3)
#Author: Sophia Tan
###################################################################################################

rm(list=ls())
source("configuration.R")

vacc <- readRDS("data/vaccination_coverage_data.RDS")
dates <- read_csv("data/weeks_months_data.csv")

hosp_data <- readRDS("data/hosp_death_rate_week_month_rates.RDS") %>% filter(weeks_since_Jan2020 >=48)
res_12_18 <- readRDS("results/alternative/main/res_12_18_sim.RDS") %>% filter(weeks_since_Jan2020>=48)
res_18_50 <- readRDS("results/alternative/main/res_18_50_sim.RDS") %>% filter(weeks_since_Jan2020>=48)
res_50_65 <- readRDS("results/alternative/main/res_50_65_sim.RDS") %>% filter(weeks_since_Jan2020>=48)
res_65 <- readRDS("results/alternative/main/res_65_sim.RDS") %>% filter(weeks_since_Jan2020>=48)

obs_cases <- hosp_data %>% filter(age_hand_cut!="[0,12)") %>% group_by(weeks_since_Jan2020) %>% summarise(cases=sum(cases))
cases <- rbind(res_12_18, res_18_50, res_50_65, res_65) %>% group_by(weeks_since_Jan2020) %>% summarise_all(sum) %>%
  mutate(obs = obs_cases$cases, vacc_cum = vacc$vacc_cum)
hosp <- total_res_hosp %>% filter(age!="[12,18)") %>% select(!age) %>% group_by(weeks_since_Jan2020) %>% summarise_all(sum) %>%
  mutate(vacc_cum = vacc$vacc_cum_18up)
death <- total_res_death %>% filter(age!="[12,18)") %>% select(!age) %>% group_by(weeks_since_Jan2020) %>% summarise_all(sum) %>%
  mutate(vacc_cum = vacc$vacc_cum_18up)

scalar1 <- max(cases$ub)/100
plot1 <- cases %>%
  ggplot(aes(x=weeks_since_Jan2020)) +
  geom_ribbon(aes(ymax=ub, ymin=lb), fill="grey80") +
  geom_line(aes(y=obs, color="Observed outcome")) +
  geom_line(aes(y=mean, color="Predicted outcome")) +
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

scalar2 <- max(hosp$ub)/100
plot2 <- hosp %>%
  ggplot(aes(weeks_since_Jan2020)) +
  geom_ribbon(aes(ymax=ub, ymin=lb), fill="grey80") +
  geom_line(aes(y=actual_hosp, color="Observed outcome")) +
  geom_line(aes(y=pred, color="Predicted outcome")) +
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

scalar3 <- scalar <- max(death$ub)/100
plot3 <- death %>%
  ggplot(aes(weeks_since_Jan2020)) +
  geom_ribbon(aes(ymax=ub, ymin=lb), fill="grey80") +
  geom_line(aes(y=actual_hosp, color="Observed outcome")) +
  geom_line(aes(y=pred, color="Predicted outcome")) +
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

library(devEMF)
emf(file="figures/all_alternative_model_results.emf", width = 10, height=4)
(plot1+plot2+plot3+
    plot_layout(guides = "collect", nrow=1) & theme(legend.position = "bottom"))
dev.off()

(plot1+plot2+plot3+
    plot_layout(guides = "collect", nrow=1) & theme(legend.position = "bottom")) %>%
  ggsave(filename="figures/all_alternative_model_results-fig3.jpg", width = 10, height=4)

