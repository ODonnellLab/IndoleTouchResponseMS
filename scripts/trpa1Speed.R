library(tidyverse)
library(ODLabPlotTools)
theme_set(theme_classic())

OPtnaA_216 <- read_csv('/Users/mo555/Library/CloudStorage/GoogleDrive-odonnell.wormlab@gmail.com/Shared\ drives/O\'Donnell\ Lab/User_folders/Madhu/anterior\ touch/2024-02-16A/Results/Results_aligned.csv')
OPtnaA_220 <- read_csv('/Users/mo555/Library/CloudStorage/GoogleDrive-odonnell.wormlab@gmail.com/Shared\ drives/O\'Donnell\ Lab/User_folders/Madhu/anterior\ touch/2024-02-20/Results/Results_aligned.csv')
OPtnaA_202 <- read_csv('/Users/mo555/Library/CloudStorage/GoogleDrive-odonnell.wormlab@gmail.com/Shared\ drives/O\'Donnell\ Lab/User_folders/Madhu/anterior\ touch/2024-02-02/Results/Results_aligned.csv')



alldata <- rbind(OPtnaA_220,
                 OPtnaA_202,
                 OPtnaA_216)

trpaData <-  alldata %>%
  filter(genotype %in% c("WT", "cest-1.2(syb3928)", "trpa-1(ok999)"),
         food == "OP50") |>
  mutate(realSpeed = case_when(
    basename(dirname(dirname(datafile))) %in% c("2024-02-26", "2024-03-01") ~ realSpeed*2,
    TRUE ~ realSpeed)) |>
  group_by(video_pref, worm_index, genotype) |>
  filter(!any(abs(realSpeed) > 500),
         realTime > 0.25,
         realTime < 2,
         realSpeed < 0)


trpap1 <-  trpaData %>%
  group_by(video_pref, worm_index, genotype, strain) %>%
  summarize(meanSpeed = abs(mean(realSpeed, na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(genotype = fct_relevel(genotype, "WT")) %>%
  ggplot(aes(x = genotype, y = meanSpeed)) +
  geom_boxplot(outlier.shape = NA, width = 0.5) +
  ggbeeswarm::geom_quasirandom(aes(color = genotype), width = 0.125, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", size = 0.2,
               position = position_nudge(x = -0.3)) +
  scale_color_manual(values = c("darkgrey", "skyblue", "gold")) +
coord_cartesian(ylim = c(0,450))
  #                 expand=c(0,0)) #+
  guides(x = "none",
         y = "none") #+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())


trpap2 <- trpaData %>%
  ungroup() %>%
  mutate(genotype = fct_relevel(genotype, "WT")) %>%
  ggplot(aes(x = realTime,
             y = abs(realSpeed))) +
  geom_path(aes(x = realTime,
                group = interaction(video_pref, worm_index),
                color = genotype),
            alpha = 0.5) +
  scale_x_continuous(limits = c(0.25,2)) +
  coord_cartesian(xlim = c(0.25,2),
                  ylim = c(0,450),
                  expand=0) +
  scale_color_manual(values = c("darkgrey", "skyblue", "gold")) +
  geom_smooth(aes(colour = genotype), method = "lm") +
  guides(color = "none") +
  labs(y = "reversal speed \n(micron/sec)",
       x = "time (s)")

library(patchwork)

trpap2 + trpap1 + plot_layout(widths =c(6,1))


### plotting head bending angle of trpa-1:
###

# first let's align with max-head bend angle:
max_bends <- trpaData %>%
  ungroup() %>%
  filter(realTime > 0.25 & realTime < 3) %>%
  group_by(genotype, video_pref, worm_index) %>%
  filter(abs(curvature_mean_neck) == max(abs(curvature_mean_neck))) %>%
  select(maxCurveTime = realTime,
         curvature_mean_neck) %>%
  mutate(flip = case_when(curvature_mean_neck < 0 ~ 1,
                          TRUE ~ 0)) %>%
  select(genotype, video_pref, worm_index, maxCurveTime, flip)

trpaData %>%
  full_join(., max_bends) %>%
  select(genotype, video_pref, worm_index, curvature_mean_neck, realTime, d_curvature_mean_head,maxCurveTime, flip) %>%
  mutate(CurveTime = realTime - maxCurveTime,
         curvature_mean_neck = case_when(
           flip == 1 ~ -curvature_mean_neck,
           TRUE ~ curvature_mean_neck),
         curvature_mean_neck = case_when(
           flip == 1 ~ -curvature_mean_neck,
           TRUE ~ curvature_mean_neck)) %>%
  ungroup() %>%
  mutate(genotype = fct_relevel(genotype, "WT")) %>%
  ggplot(aes(x = CurveTime,
             y = d_curvature_mean_head)) +
  geom_path(aes(x = CurveTime,
                group = interaction(video_pref, worm_index),
                color = genotype),
            alpha = 0.5) +
  geom_smooth(aes(colour = genotype),method = "loess", span = 0.1) +
  #scale_x_continuous(limits = c(0.25,2)) +
  coord_cartesian(#xlim = c(0.25,2),
                  #ylim = c(0,450),
                  expand=0) +
  scale_color_manual(values = c("darkgrey", "skyblue", "gold")) +
  #guides(color = "none") +
  labs(y = "tbd",
       x = "time (s)")



trpaData %>%
  filter(motion_mode == -1) %>%
  ggplot(aes(x = curvature_mean_head,
                        y = curvature_mean_neck)) +
  geom_path(aes(group = interaction(video_pref, worm_index),
                color = genotype))
