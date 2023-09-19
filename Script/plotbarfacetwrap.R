library(WRS2)
library(car)


dfpourcentage %>% ggplot(aes(x = condition , y = IMVCdif, fill = condition)) +
  stat_summary(
    fun.data = mean_se,
    position = position_dodge(0.65),
    geom = "errorbar",
    width = 0.3
  ) +
  stat_summary(
    fun.y = mean ,
    position = position_dodge(width = 0.65),
    geom = "bar",
    width = 0.6,
    color = "black"
  ) +
  scale_fill_manual(values = c("black", "grey")) +
  ylab("Percentage change in KE IMVC from PRE to MID / POST / POST48 (%)") +
  # stat_summary(aes(label = round(..y.., 2)),
  #   geom = "text",
  #   fun.y = mean,
  #   size = 4) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5 , size = 12 , face = "bold") ,
    axis.text = element_text(size = 7) ,
    axis.title = element_text(size = 8 , face = "bold") ,
    legend.position = "right" ,
    legend.title = element_text(size = 8 , face = "bold") ,
    legend.text = element_text(size = 6) ,
    legend.background = element_rect(color = "black" , size = 0.1)
  ) +
  facet_grid(cols = vars(instant)) +
  stat_compare_means(
  label = "p.format" ,
  method = "t.test",
  paired = T ,
  bracket.size = 10
)


