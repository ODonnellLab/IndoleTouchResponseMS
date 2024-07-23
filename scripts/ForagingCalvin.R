library(tidyverse)
theme_set(theme_classic())
BaseFolder <- dirname(file.choose())
files <- fs::dir_ls(BaseFolder, glob = "*Results_extracted_tracks.csv", recurse = TRUE)

files <- c("/Users/mo555/Library/CloudStorage/GoogleDrive-odonnell.wormlab@gmail.com/Shared drives/O'Donnell Lab/User_folders/Calvin/Behavior/2024.01.30/Results/Results_extracted_tracks.csv",
           "/Users/mo555/Library/CloudStorage/GoogleDrive-odonnell.wormlab@gmail.com/Shared drives/O'Donnell Lab/User_folders/Calvin/Behavior/2024.02.02/Results/Results_extracted_tracks.csv")
           #"/Users/mo555/Library/CloudStorage/GoogleDrive-odonnell.wormlab@gmail.com/Shared drives/O'Donnell Lab/User_folders/Calvin/Behavior/2024.04.16_off_food_op50/Results/Results_extracted_tracks.csv",
           #"/Users/mo555/Library/CloudStorage/GoogleDrive-odonnell.wormlab@gmail.com/Shared drives/O'Donnell Lab/User_folders/Calvin/Behavior/2024.04.17_foraging/Results/Results_extracted_tracks.csv")



all_tracks <- map_df(files, read_csv, .id = "folder")

all_tracks %>%
  mutate(motion_mode = factor(motion_mode)) %>%
  filter(
                      motion_mode %in% c(-1,1,0),
                      coord_x_head > 2500,
                      coord_x_head < 10000,
                      coord_y_head > 3750,
                      coord_y_head < 8000,
                      speed > -400, speed < 400) %>%
  ggplot(aes(x = coord_x_head, y = coord_y_head)) +
  geom_point(aes(color = motion_mode), alpha = 0.3) +
  facet_grid(food~genotype) #+
  scale_color_viridis_d(option = "magma")

all_tracks <- all_tracks %>%
  mutate(date = basename(dirname(dirname(folder))))


# off food reversal and forward speeds
motion_labels <- c("Reverse locomotion", "Forward locomotion")
names(motion_labels) <- c("-1","1")

p1 <- all_tracks %>%
  filter(#motion_mode %in% c(-1, 1),
    motion_mode == -1,
  coord_x_head > 2500,
  coord_x_head < 10000,
  coord_y_head > 3750,
  coord_y_head < 8000,
  (speed < -25 | speed > 25)
  ) %>%
  group_by(video_pref, worm_index, genotype, motion_mode, food, folder) %>%
  summarize(mean_speed = mean(speed, na.rm = TRUE)) %>%
  mutate( genotype = case_when(
    is.na(genotype) ~ "cest-1.2(syb3928)",
    TRUE ~ genotype )) %>%
  #speed < -50, speed > -600) %>%
  ungroup() %>%
  mutate(genotype = fct_relevel(genotype, c("WT", "cest-1.2(syb3928)"))) %>%
  ggplot(aes(x = genotype, y = abs(mean_speed), fill = genotype)) +
  stat_summary(geom = "bar", fun = "mean", width = 0.75) +
  stat_summary(geom = "errorbar", fun.data = "mean_se", width = 0.25) +
  ggbeeswarm::geom_quasirandom(width = 0.25, alpha = 0.25, size = 0.5) +
  facet_grid(.~motion_mode, labeller = labeller(motion_mode = motion_labels)) +
  scale_fill_manual(values = c("grey", "orange", "purple")) +
  scale_alpha_manual(values = c(1,0.5)) +
  coord_cartesian(ylim = c(0,300)) +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank()) +
  scale_y_continuous(expand = c(0,0))

p1
p1 + ODLabPlotTools::theme_black()

all_tracks %>% filter(motion_mode %in% c(-1,1,0),
         coord_x_head > 2500,
         coord_x_head < 10000,
         coord_y_head > 3750,
         coord_y_head < 8000) %>%
    mutate( genotype = case_when(
      is.na(genotype) ~ "cest-1.2(syb3928)",
      TRUE ~ genotype),
      food = case_when(
        is.na(food) ~ "OP50",
        TRUE ~ food),
      genotype = fct_relevel(genotype, "WT") ) %>%
  filter( food == "OP50") %>%
  ggplot(aes(x = speed)) +
  geom_density(aes(color = genotype), adjust = 3/4) +
  #geom_point(alpha = 0.2) +
  facet_grid(folder~food) +
  scale_x_continuous(limits = c(-400,400))


on_food <- c("/Users/mo555/Library/CloudStorage/GoogleDrive-odonnell.wormlab@gmail.com/Shared drives/O'Donnell Lab/User_folders/Calvin/Behavior/2024.04.18_onfood/temporary/Results/Results_extracted_tracks.csv")

all_tracks <- map_df(on_food, read_csv, .id = "folder")

# on food reversal speeds:
all_tracks %>%
  filter(motion_mode %in% c(-1,1),
         # coord_x_head > 2500,
         # coord_x_head < 10000,
         # coord_y_head > 3750,
         coord_y_head < 5000
  ) %>%
  group_by(video_pref, worm_index, genotype, motion_mode, food, folder) %>%
  summarize(mean_speed = mean(speed, na.rm = TRUE)) %>%
  mutate( genotype = case_when(
    is.na(genotype) ~ "cest-1.2(syb3928)",
    TRUE ~ genotype )) %>%
  #speed < -50, speed > -600) %>%
  ungroup() %>%
  mutate(genotype = fct_relevel(genotype, c("WT", "cest-1.2(syb3928)"))) %>%
  ggplot(aes(x = food, y = abs(mean_speed), group = food, fill = genotype)) +
  stat_summary(geom = "bar", fun = "mean", aes(alpha = food)) +
  stat_summary(geom = "errorbar", fun.data = "mean_se", width = 0.25) +
  ggbeeswarm::geom_quasirandom(width = 0.25, alpha = 0.25, size = 0.25) +
  facet_grid(.~genotype) +
  scale_fill_manual(values = c("grey", "orange", "purple")) +
  scale_alpha_manual(values = c(1,0.5)) +
  coord_cartesian(ylim = c(0,250)) +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        strip.text = element_blank()) +
  labs(title = "Reversal speeds on food",
       y = expression("Reversal speed ("~mu~"m / sec)"))


