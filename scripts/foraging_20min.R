### foraging behavior analysis:
###
library(tidyverse)
BaseFolder <- dirname(file.choose())
files <- fs::dir_ls(BaseFolder, glob = "*Results_foraging_data.csv", recurse = TRUE)

# 1/30 and 2/02 dates
foraging <- map_df(files[2:3], read_csv, .id = "folder")

foraging %>%
  group_by(genotype, worm_index, rev_freq, rev_speed, fwd_speed,time_bin_min) %>%
  nest() %>%
ggplot(aes(x = genotype, y = fwd_speed)) +
  geom_boxplot() #+ facet_wrap(~time_bin_min)

time_labels <- c("5-10 min", "10-15 min", "15-20 min")
names(time_labels) <- c("10", "15", "20")

foraging %>%
  group_by(genotype, worm_index, rev_freq, rev_speed, fwd_speed,time_bin_min) %>%
  nest() %>%
  filter(
    #rev_freq > 0
    time_bin_min != "5"
    ) %>%
  ungroup() %>%
  mutate(genotype = fct_relevel(genotype, "WT")) %>%
  ggplot(aes(x = genotype, y = rev_freq,  fill = genotype)) +
  stat_summary(geom = "bar", fun = "mean") +
  stat_summary(geom = "errorbar", fun.data = "mean_se", width = 0.25) +
  ggbeeswarm::geom_quasirandom(width = 0.25, alpha = 0.25, size = 0.25) +
  facet_grid(.~time_bin_min, labeller = labeller(time_bin_min = time_labels)) +
  scale_fill_manual(values = c("grey", "orange", "purple")) +
  theme(axis.text.x = element_blank()) +
  labs(title = "Off food foraging behavior",
       y = "Reversal frequency (per minute)",
       x = "") +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(ylim = c(0,8)) +
  #theme_black() +
  guides(fill = "none")





foraging %>%
  group_by(genotype, worm_index, rev_freq, rev_speed, fwd_speed) %>%
  nest() %>%
  lm(data = ., formula =  fwd_speed~ genotype) %>% summary()



