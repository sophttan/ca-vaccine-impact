###################################################################################################
#Title: Plot average vaccine efficacy (Figure A1)
#Author: Sophia Tan
###################################################################################################

source("configuration.R")

protected <- data.frame(weeks = seq(0, 94))
protected <- protected %>% mutate(jj=0.66,
                                  p1=0.52,
                                  p2=ifelse(weeks < 3, 0.52,
                                            ifelse(weeks >= 120/7 + 3, 0.77, 0.95)),
                                  m1=0.82,
                                  m2=ifelse(weeks < 4, 0.82,
                                            ifelse(weeks >= 120/7 + 4, 0.92, 0.94)))

# plot average efficacy over time
efficacy <- ggplot(protected %>% filter(weeks <= 40), aes(weeks)) +
  geom_line(aes(y=jj*100, color="Ad26.COV2.S")) +
  geom_line(aes(y=p1*100, color="BNT162b2, 1 dose")) +
  geom_line(aes(y=p2*100, color="BNT162b2, 2 doses")) +
  geom_line(aes(y=m1*100, color="mRNA-1273, 1 dose")) +
  geom_line(aes(y=m2*100, color="mRNA-1273, 2 doses")) +
  ylab("Effectiveness (%)") +
  ylim(c(0,100)) +
  scale_x_continuous(name="Weeks after first dose") +
  theme(legend.title = element_blank())

ggsave(efficacy, filename="figures/vaccine-effectiveness-figA1.jpg", dpi=300, width=6, height=5)
