library(ODLabPlotTools)
AntTouch <- read_csv('datasets/cest1.2_anterior_touch_merged.csv') %>%
  mutate(genotype = fct_relevel(genotype, "N2"))


AntTouch %>%
  filter(date == "2/25/2023",
         genotype == "N2") %>%
  ggplot(aes(x = food, y = bends)) +
  stat_summary(aes(fill = food), geom = "bar", fun = "mean", width = 0.75) +
  stat_summary(geom = "errorbar", fun.data = "mean_se", width = 0.25, colour = "black") +
  #theme_black() +
  scale_y_continuous(expand = c(0,0)) +
  guides(fill = "none") +
  scale_fill_manual(values = c("grey", "darkgreen"))

glo1_dates <- AntTouch %>% filter(genotype == "glo-1") %>%
  group_by(date) %>% tally() %>% pull(date)

AntTouch %>%
  filter(date %in% glo1_dates,
         genotype %in% c("N2", "glo-1"),
         food == "OP50") %>%
  ggplot(aes(x = genotype, y = bends)) +
  stat_summary(aes(fill = genotype), geom = "bar", fun = "mean", width = 0.75) +
  stat_summary(geom = "errorbar", fun.data = "mean_se", width = 0.25, colour = "white") +
  theme_black() +
  guides(fill = "none") +
  scale_fill_manual(values = c("grey", "blue"))

cest12_dates <- AntTouch %>% filter(genotype == "cest-1.2") %>%
  group_by(date) %>% tally() %>% pull(date)

AntTouch %>%
  filter(date %in% glo1_dates,
         genotype %in% c("N2", "glo-1", "cest-1.2"),
         food == "OP50") %>%
  mutate(genotype = fct_relevel(genotype, "N2", "glo-1")) %>%
  ggplot(aes(x = genotype, y = bends)) +
  stat_summary(aes(fill = genotype), geom = "bar", fun = "mean", width = 0.75) +
  stat_summary(geom = "errorbar", fun.data = "mean_se", width = 0.25, colour = "white") +
  theme_black() +
  guides(fill = "none") +
  scale_fill_manual(values = c("grey", "blue", "orange"))

read_csv('datasets/cest-1.2_rescue_merged.csv') %>%
  mutate(Strain = fct_relevel(Strain, "WT", "Lof", "Gut", "Gut_SN")) %>%
  ggplot(aes(x = Strain, y = bends)) +
  stat_summary(aes(fill = Strain), geom = "bar", fun = "mean", width = 0.75) +
  stat_summary(geom = "errorbar", fun.data = "mean_se", width = 0.25, colour = "white") +
  theme_black() +
  guides(fill = "none") +
  scale_fill_manual(values = c("grey", "orange", "darkorange2", "yellow3"))

trpa1_dates <- AntTouch %>% filter(genotype == "trpa-1") %>%
  group_by(date) %>% tally() %>% pull(date)

AntTouch %>%
  filter(date %in% trpa1_dates,
         genotype %in% c("N2", "cest-1.2", "trpa-1"),
         food == "OP50") %>%
  mutate(genotype = fct_relevel(genotype, "N2", "glo-1")) %>%
  ggplot(aes(x = genotype, y = bends)) +
  stat_summary(aes(fill = genotype), geom = "bar", fun = "mean", width = 0.75) +
  stat_summary(geom = "errorbar", fun.data = "mean_se", width = 0.25, colour = "white") +
  theme_black() +
  guides(fill = "none") +
  scale_fill_manual(values = c("grey", "orange", "purple"))

