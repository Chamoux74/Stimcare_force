plot <-ggplot(DFIMVCmax1,
              aes(
                x = as.numeric(instant_mesure),
                y = IMVC,
                color = condition ,
                group = interaction(sujet, condition)
              )) +
  geom_smooth(
    se = F ,
    method = "lm",
    fill = NA ,
    size = 0.4,
    alpha = 0.3
  ) +
  geom_point()

ggplot(data = DFIMVCmax1, aes(
  x = as.numeric(instant_mesure),
  y = IMVC,
  color = condition
)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Temps", y = "Force Maximale") +
  facet_wrap( ~ sujet) +
  stat_cor(aes(label = after_stat(rr.label), color =condition), geom = "label" , size = 3)
