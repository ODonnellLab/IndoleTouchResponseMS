library(tidyverse)
theme_set(theme_classic())

OPtnaA_226 <- read_csv('/Users/mo555/Library/CloudStorage/GoogleDrive-odonnell.wormlab@gmail.com/Shared\ drives/O\'Donnell\ Lab/User_folders/Madhu/anterior\ touch/2024-02-26/Results/Results_aligned.csv')
OPtnaA_220 <- read_csv('/Users/mo555/Library/CloudStorage/GoogleDrive-odonnell.wormlab@gmail.com/Shared\ drives/O\'Donnell\ Lab/User_folders/Madhu/anterior\ touch/2024-02-20/Results/Results_aligned.csv')
OPtnaA_301 <- read_csv('/Users/mo555/Library/CloudStorage/GoogleDrive-odonnell.wormlab@gmail.com/Shared\ drives/O\'Donnell\ Lab/User_folders/Madhu/anterior\ touch/2024-03-01/Results/Results_aligned.csv')



# looks like 0301 and 0226 used the wrong (0.5x) resolution.
alldata <- rbind(OPtnaA_220,
      OPtnaA_226,
      OPtnaA_301) |>
  mutate(realSpeed = case_when(
    basename(dirname(dirname(datafile))) %in% c("2024-02-26", "2024-03-01") ~ realSpeed*2,
    TRUE ~ realSpeed),
    x_rotated = case_when(
      basename(dirname(dirname(datafile))) %in% c("2024-02-26", "2024-03-01") ~ x_rotated*2,
      TRUE ~ x_rotated)) %>%
  filter(!worm_index %in% c(3379, 5587)) # worm was mis-tracked

#### analysis of reversal speed ####

modData <- alldata %>%
  filter(genotype == "WT") |>
  group_by(video_pref, worm_index, food) |>
  filter(!any(abs(realSpeed) > 500),
         realTime > 0.25,
         realTime < 2,
         realSpeed < 0)

p1 <- modData %>%
  summarize(meanSpeed = abs(mean(realSpeed, na.rm = TRUE))) %>%
  ggplot(aes(x = food, y = meanSpeed)) +
  geom_boxplot(outlier.shape = NA, width = 0.5) +
  ggbeeswarm::geom_quasirandom(aes(color = food), width = 0.25, alpha = 0.5) +
  stat_summary(aes(x = as.numeric(factor(food)) - 0.4),
                   fun.data = "mean_cl_boot",
               size = 0.05) +
  scale_color_manual(values = c("darkgrey", "darkgreen")) +
  coord_cartesian(ylim = c(0,450),
                  xlim = c(0.5,2.5),
                  expand=0) +
  guides(x = "none",
         y = "none",
         color = "none") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())


p2 <- alldata %>%
  filter(genotype == "WT") |>
  group_by(video_pref, worm_index, food) |>
  filter(!any(abs(realSpeed) > 500),
         realSpeed < 0) |>
  ggplot(aes(x = realTime,
             y = abs(realSpeed))) +
  stat_smooth(geom = "line",
              aes(x = realTime,
                 group = interaction(video_pref, worm_index),
                 color = food),
            linewidth = 0.25,
            alpha = 0.5,
            se = FALSE,
            span = .25,
            method = "loess") +
  coord_cartesian(xlim = c(0.25,2),
                  ylim = c(0,450),
                  expand=0) +
  scale_color_manual(values = c("darkgrey", "darkgreen")) +
  geom_smooth(aes(colour = food),
              method = "loess",
              span = 0.5) +
  guides(color = "none") +
  labs(y = "reversal speed \n(micron/sec)",
       x = "time (s)")

library(patchwork)

p2 + p1 + plot_layout(widths =c(5,1))


mod <- lme4::lmer(data = (modData %>%
                    summarize(meanSpeed = abs(mean(realSpeed, na.rm = TRUE)))),
                  formula = meanSpeed ~ food + (1|video_pref))
bayesmod <- brms::brm(modData %>%
                        summarize(meanSpeed = abs(mean(realSpeed, na.rm = TRUE))),
          family = gaussian(),
          formula = brms::bf(meanSpeed ~ food + (1|video_pref)),
          chains = 4,
          iter = 4000)

library(emmeans)

emmeans(mod, "food") %>%
  contrast(method = "pairwise")

emmeans(bayesmod, "food") %>%
  contrast(method = "pairwise")

#### distance/duration ####
####
####
####
#Lets check to make sure the tracking doesn't vary by food.
#
p <- alldata %>%
  filter(genotype == "WT") %>%
  group_by(worm_index, video_pref, food, F0, Fs) %>%
  nest() %>%
  filter(worm_index == 5587)
  mutate(delta = F0-Fs) %>%
  ggplot(aes(x = food, y = delta)) +
  geom_point(aes(group = worm_index))

distances <- alldata %>%
  filter(genotype == "WT",realSpeed < 0) %>%
  group_by(worm_index, video_pref, food) %>%
  summarize(distance = max(x_rotated))
#
distancep1 <- alldata %>%
  filter(genotype == "WT",
         realSpeed < 0) |>
  group_by(video_pref, worm_index) |>
  filter(!any(abs(realSpeed) > 500)) |>
  ggplot(aes(y = x_rotated, x = realTime)) +
  geom_path(aes(group = interaction(video_pref, worm_index),
                color = food),
            alpha = 0.5) +
  scale_y_continuous(limits = c(0,4000)) +
  scale_color_manual(values = c("grey", "darkgreen")) +
  geom_smooth(aes(colour = food)) +
  guides(color = "none") +
  labs(y = "Distance covered (microns)",
       x = "Time (s)")

distancep2 <- ggplot(distances, aes(y = distance, x = food)) +
  geom_boxplot(outlier.shape = NA, width = 0.5) +
  ggbeeswarm::geom_quasirandom(orientation = 'x',
                               aes(color = food), width = 0.25, alpha = 0.5) +
  stat_summary(aes(x = as.numeric(factor(food)) - 0.4),
               fun.data = "mean_cl_boot",
               size = 0.05) +
  scale_color_manual(values = c("darkgrey", "darkgreen")) +
  guides(x = "none",
         y = "none",
         color = "none") +
  scale_y_continuous(limits = c(0,4000)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

library(patchwork)


p <- distancep1 + distancep2 + plot_layout(widths =c(4,1))



##### distance plot for first 2 seconds
distances <- alldata %>%
  filter(genotype == "WT",realSpeed < 0,realTime <=2) %>%
  group_by(worm_index, video_pref, food) %>%
  summarize(distance = max(x_rotated))
#
distancep1 <- alldata %>%
  filter(genotype == "WT",
         realSpeed < 0,
         realTime <= 2) |>
  group_by(video_pref, worm_index) |>
  filter(!any(abs(realSpeed) > 500)) |>
  ggplot(aes(y = realSpeed, x = x_rotated)) +
  geom_path(aes(group = interaction(video_pref, worm_index),
                color = food),
            alpha = 0.5) +
  scale_x_continuous(limits = c(0,1000)) +
  scale_color_manual(values = c("grey", "darkgreen")) +
  geom_smooth(aes(colour = food)) +
  guides(color = "none") +
  labs(y = "reversal speed \n(micron/sec)",
       x = "distance covered (microns)")

distancep2 <- ggplot(distances, aes(x = distance, y = food)) +
  geom_boxplot(outlier.shape = NA, width = 0.5) +
  ggbeeswarm::geom_quasirandom(orientation = 'x',
                               aes(color = food), width = 0.25, alpha = 0.5) +
  stat_summary(aes(y = as.numeric(factor(food)) - 0.4),
               fun.data = "mean_cl_boot",
               size = 0.05) +
  scale_color_manual(values = c("darkgrey", "darkgreen")) +
  guides(x = "none",
         y = "none",
         color = "none") +
  scale_x_continuous(limits = c(0,1000)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

library(patchwork)


p <- distancep2 / distancep1 + plot_layout(heights =c(1,4))


durations <- alldata %>%
  filter(genotype == "WT",realSpeed < 0) %>%
  group_by(worm_index, video_pref, food) %>%
  summarize(duration = max(realTime))
#
distancep1 <- alldata %>%
  filter(genotype == "WT",
         realSpeed < 0,
         realTime <= 2) |>
  group_by(video_pref, worm_index) |>
  filter(!any(abs(realSpeed) > 500)) |>
  ggplot(aes(y = realSpeed, x = x_rotated)) +
  geom_path(aes(group = interaction(video_pref, worm_index),
                color = food),
            alpha = 0.5) +
  scale_x_continuous(limits = c(0,1000)) +
  scale_color_manual(values = c("grey", "darkgreen")) +
  geom_smooth(aes(colour = food)) +
  guides(color = "none") +
  labs(y = "reversal speed \n(micron/sec)",
       x = "distance covered (microns)")

distancep2 <- ggplot(distances, aes(x = distance, y = food)) +
  geom_boxplot(outlier.shape = NA, width = 0.5) +
  ggbeeswarm::geom_quasirandom(orientation = 'x',
                               aes(color = food), width = 0.25, alpha = 0.5) +
  stat_summary(aes(y = as.numeric(factor(food)) - 0.4),
               fun.data = "mean_cl_boot",
               size = 0.05) +
  scale_color_manual(values = c("darkgrey", "darkgreen")) +
  guides(x = "none",
         y = "none",
         color = "none") +
  scale_x_continuous(limits = c(0,1000)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

library(patchwork)


p <- distancep2 / distancep1 + plot_layout(heights =c(1,4))
