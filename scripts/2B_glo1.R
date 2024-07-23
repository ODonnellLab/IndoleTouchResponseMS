library(tidyverse)
theme_set(theme_classic())

OPtnaA_226 <- read_csv('/Users/mo555/Library/CloudStorage/GoogleDrive-odonnell.wormlab@gmail.com/Shared\ drives/O\'Donnell\ Lab/User_folders/Madhu/anterior\ touch/2024-02-26/Results/Results_aligned.csv')
OPtnaA_220 <- read_csv('/Users/mo555/Library/CloudStorage/GoogleDrive-odonnell.wormlab@gmail.com/Shared\ drives/O\'Donnell\ Lab/User_folders/Madhu/anterior\ touch/2024-02-20/Results/Results_aligned.csv')
OPtnaA_301 <- read_csv('/Users/mo555/Library/CloudStorage/GoogleDrive-odonnell.wormlab@gmail.com/Shared\ drives/O\'Donnell\ Lab/User_folders/Madhu/anterior\ touch/2024-03-01/Results/Results_aligned.csv')



alldata <- rbind(OPtnaA_226,
      OPtnaA_301)

gloData <-  alldata %>%
  filter(genotype %in% c("WT", "glo-1(zu391)"),
                         food == "OP50") |>
  mutate(realSpeed = case_when(
    basename(dirname(dirname(datafile))) %in% c("2024-02-26", "2024-03-01") ~ realSpeed*2,
    TRUE ~ realSpeed)) |>
  group_by(video_pref, worm_index, genotype) |>
  filter(!any(abs(realSpeed) > 500),
         realTime > 0.25,
         realTime < 2,
         realSpeed < 0)


glop1 <-  gloData %>%
  group_by(video_pref, worm_index, genotype, strain) %>%
  summarize(meanSpeed = abs(mean(realSpeed, na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(genotype = fct_relevel(genotype, "WT")) %>%
  ggplot(aes(x = genotype, y = meanSpeed)) +
  geom_boxplot(outlier.shape = NA, width = 0.5) +
  ggbeeswarm::geom_quasirandom(aes(color = genotype), width = 0.25, alpha = 0.5) +
  stat_summary(aes(x = as.numeric(factor(genotype)) - 0.4),
               fun.data = "mean_cl_boot",
               size = 0.05) +
  scale_color_manual(values = c("darkgrey", "darkblue")) +
  coord_cartesian(ylim = c(0,450),
                  xlim = c(0.5,2.5),
                  expand=0) +
  guides(x = "none",
         y = "none",
         color = "none") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())


glop2 <- gloData %>%
  ungroup() %>%
  mutate(genotype = fct_relevel(genotype, "WT")) %>%
  ggplot(aes(x = realTime,
             y = abs(realSpeed))) +
  geom_path(aes(x = realTime,
                group = interaction(video_pref, worm_index),
                color = genotype),
            alpha = 0.25) +
  scale_x_continuous(limits = c(0.25,2)) +
  coord_cartesian(xlim = c(0.25,2),
                  ylim = c(0,450),
                  expand=0) +
  scale_color_manual(values = c("darkgrey", "darkblue")) +
  geom_smooth(aes(colour = genotype), method = "lm") +
  guides(color = "none") +
  labs(y = "reversal speed \n(micron/sec)",
       x = "time (s)")

library(patchwork)

glop2 + glop1 + plot_layout(widths =c(5,1))
