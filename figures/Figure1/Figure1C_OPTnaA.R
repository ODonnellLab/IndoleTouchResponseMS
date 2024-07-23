library(ODLabPlotTools)
AntTouch <- read_csv('datasets/TouchResponsesData.csv') %>%
  mutate(genotype = fct_relevel(genotype, "N2"))



AntTouch %>%
  filter(#date == "2/25/2023",
         genotype == "N2",
         food %in% c("OP50")) %>%
  ggplot(aes(x = food, y = bends)) +
  stat_summary(aes(fill = food), geom = "bar", fun = "mean", width = 0.75) +
  stat_summary(geom = "errorbar", fun.data = "mean_se", width = 0.25, colour = "black") +
  #theme_black() +
  scale_y_continuous(expand = c(0,0)) +
  guides(fill = "none") +
  scale_fill_manual(values = c("grey", "darkgreen"))
