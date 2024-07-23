library(tidyverse)
library(psdr)
library(ggrepel)
theme_set(theme_classic())
Touch1219<- read_csv('/Users/mo555/Library/CloudStorage/GoogleDrive-odonnell.wormlab@gmail.com/Shared\ drives/O\'Donnell\ Lab/User_folders/Madhu/anterior\ touch/2023-12-19/Results/Results_aligned.csv')
Touch1222<- read_csv('/Users/mo555/Library/CloudStorage/GoogleDrive-odonnell.wormlab@gmail.com/Shared\ drives/O\'Donnell\ Lab/User_folders/Madhu/anterior\ touch/2023-12-22/Results/Results_aligned.csv')
Touch1220<- read_csv('/Users/mo555/Library/CloudStorage/GoogleDrive-odonnell.wormlab@gmail.com/Shared\ drives/O\'Donnell\ Lab/User_folders/Madhu/anterior\ touch/2023-12-20/Results/Results_aligned.csv') %>%
  filter(user == "Madhu")
Touch1226<- read_csv('/Users/mo555/Library/CloudStorage/GoogleDrive-odonnell.wormlab@gmail.com/Shared\ drives/O\'Donnell\ Lab/User_folders/Madhu/anterior\ touch/2023-12-26/Results/Results_aligned.csv')
Touch1229<- read_csv('/Users/mo555/Library/CloudStorage/GoogleDrive-odonnell.wormlab@gmail.com/Shared\ drives/O\'Donnell\ Lab/User_folders/Madhu/anterior\ touch/2023-12-29/Results/Results_aligned.csv')
Touch1218<- read_csv('/Users/mo555/Library/CloudStorage/GoogleDrive-odonnell.wormlab@gmail.com/Shared\ drives/O\'Donnell\ Lab/User_folders/Madhu/anterior\ touch/2023-12-18/Results/Results_aligned.csv')
Touch1228<- read_csv('/Users/mo555/Library/CloudStorage/GoogleDrive-odonnell.wormlab@gmail.com/Shared\ drives/O\'Donnell\ Lab/User_folders/Madhu/anterior\ touch/2023-12-28/Results/Results_aligned.csv')
Touch1129<- read_csv('/Users/mo555/Library/CloudStorage/GoogleDrive-odonnell.wormlab@gmail.com/Shared\ drives/O\'Donnell\ Lab/User_folders/Madhu/anterior\ touch/2023-11-29/Results/Results_aligned.csv')
Touch1213<- read_csv('/Users/mo555/Library/CloudStorage/GoogleDrive-odonnell.wormlab@gmail.com/Shared\ drives/O\'Donnell\ Lab/User_folders/Madhu/anterior\ touch/2023-12-13/Results/Results_aligned.csv')
Touch1210<- read_csv('/Users/mo555/Library/CloudStorage/GoogleDrive-odonnell.wormlab@gmail.com/Shared\ drives/O\'Donnell\ Lab/User_folders/Madhu/anterior\ touch/2023-12-10/Results/Results_aligned.csv')
Touch1211<- read_csv('/Users/mo555/Library/CloudStorage/GoogleDrive-odonnell.wormlab@gmail.com/Shared\ drives/O\'Donnell\ Lab/User_folders/Madhu/anterior\ touch/2023-12-11/Results/Results_aligned.csv') %>%
  select(-note)
Touch0116<- read_csv('/Users/mo555/Library/CloudStorage/GoogleDrive-odonnell.wormlab@gmail.com/Shared\ drives/O\'Donnell\ Lab/User_folders/Madhu/anterior\ touch/2024-01-16/30fps/Results/Results_aligned.csv') %>%
  select(-fps)
Touch011650<- read_csv('/Users/mo555/Library/CloudStorage/GoogleDrive-odonnell.wormlab@gmail.com/Shared\ drives/O\'Donnell\ Lab/User_folders/Madhu/anterior\ touch/2024-01-16/50fps/Results/Results_aligned.csv')%>%
  select(-fps)
Touch0126<- read_csv('/Users/mo555/Library/CloudStorage/GoogleDrive-odonnell.wormlab@gmail.com/Shared\ drives/O\'Donnell\ Lab/User_folders/Madhu/anterior\ touch/2024-01-26/Results/Results_aligned.csv')
Touch0130<- read_csv('/Users/mo555/Library/CloudStorage/GoogleDrive-odonnell.wormlab@gmail.com/Shared\ drives/O\'Donnell\ Lab/User_folders/Madhu/anterior\ touch/2024-01-30/Results/Results_aligned.csv')
Touch0202<- read_csv('/Users/mo555/Library/CloudStorage/GoogleDrive-odonnell.wormlab@gmail.com/Shared\ drives/O\'Donnell\ Lab/User_folders/Madhu/anterior\ touch/2024-02-02/Results/Results_aligned.csv')
Touch0205<-read_csv('/Users/mo555/Library/CloudStorage/GoogleDrive-odonnell.wormlab@gmail.com/Shared\ drives/O\'Donnell\ Lab/User_folders/Madhu/anterior\ touch/2024-02-05/Results/Results_aligned.csv')


Touch1220 |>
  ggplot(aes(x = eigen_projection_1, y = eigen_projection_2, colour = genotype)) +
  geom_point(size = 0.5) +
  facet_grid(.~genotype)

library(plotly)

Touch1222 |>
  group_by(genotype) |>
  do(p=plotly::plot_ly(.,
                       x = ~curvature_mean_head,
                       y = ~curvature_mean_neck,
                       z = ~curvature_mean_midbody,
                       color = ~food,
                       type="scatter3d",
                       mode="lines")) |>
  subplot(nrows = 1)

Touch1220 |>
  filter(genotype == "WT") |>
  group_by(genotype) %>%
  plotly::plot_ly(.,x = ~curvature_mean_head,
                       y = ~curvature_mean_neck,
                       z = ~curvature_mean_midbody,
                       color = ~food,
                       type="scatter3d",
                       mode="lines")


p <- Touch1220 |>
  group_by(worm_index) %>%
  # filter(motion_mode == -1 & lag(motion_mode == -1),
  #        realTime - lag(realTime) < .2 ) |>
  ggplot(aes(x = x_rotated, y = curvature_mean_neck, colour = genotype)) +
  geom_path(aes(group = interaction(worm_index, datafile))) +
  facet_grid(food~.) +
  scale_color_brewer(palette = "Set2")
plotly::ggplotly(p)

p <- Touch1220 |>
  group_by(worm_index) %>%
  filter(motion_mode == -1 & lag(motion_mode == -1),
         realTime - lag(realTime) < .2 ) |>
  ggplot(aes(x = realTime, y = curvature_mean_neck, colour = genotype)) +
  geom_path(aes(group = interaction(worm_index, datafile))) +
  facet_grid(food~.) +
  scale_color_brewer(palette = "Set2")
plotly::ggplotly(p)

p<- Touch1226 |>
  group_by(worm_index) %>%
  filter(motion_mode == -1 & lag(motion_mode == -1),
         realTime - lag(realTime) < .2,
         genotype == "WT") |>
  ggplot(aes(x = x_rotated, y = curvature_mean_head, colour = genotype)) +
  geom_path(aes(group = worm_index), alpha = 0.5) +
  geom_point(alpha = 0.25, size = 0.5) +
  facet_grid(food~.) +
  scale_color_brewer(palette = "Set2")
plotly::ggplotly(p)

N2OP <- c(4.3, 4.6, 4, 3.8, 4, 3.8, 5.1, 4.5, 4, 3.6, 4.4, 5, 3.3, 4.3, 3.9)
cest12OP <- c(3.9, 3.8, 3.3, 4.8, 3.4, 4.8, 4.8, 3.7, 3.5, NA, NA, 3.6, 3.1, 2.8)

library(psd)

out1 <- pspectrum(Touch1226$curvature_head[1:42], 10)

merged <- rbind(Touch1219,Touch1222,Touch1226,Touch1229) %>%
  mutate(video_pref = as.character(video_pref)) |>
  mutate(date = substr(video_pref,1,8)) |>
  group_by(worm_index, datafile) %>%
  # identify frame gaps
  mutate(frame_gap = c(1,diff(timestamp))) |>
  # mark each change of motiom_mode (will keep only those with 0 value)
  mutate(state_change = cumsum(replace_na(motion_mode != lag(motion_mode), 0))) |>
  #eliminate worms with wrong head-tail ID
  filter(first(motion_mode) == -1) |>
  # consider only reversal mode (should be unnecessary if using state_change)
  #filter(motion_mode == -1) |>
  # eliminate all worms with a substantial frame gap or change of motion state
  filter(!any(frame_gap > 3)) |>
  filter(state_change == 0) |>
  group_by(genotype, food, worm_index) %>%
  nest() %>%
  # get frequency information:
  mutate(freq = map(data, function(x) {
    curve <- x |> pull(curvature_mean_neck)
    #curve <- curve[1:30]
    time <- x |> pull(realTime)
    fit_end <- if_else(length(time) > 40,40,length(time))
    results <-  PSDIdentifyDominantFrequency(data_vector = curve[1:fit_end],
                                             sampling_frequency = 10,
                                             x_end = .75,
                                             x_increment = 0.01)
    freq = results$xval
    #phase at 0.5Hz for alignment sin(2pi(f)(t)) + cos(2pi(f)(t))
    fit = lm(curve ~ sin(2*pi*.45*time) + cos(2*pi*.45*time))
    phase = as.numeric(atan(fit$coefficients[3]/fit$coefficients[2]))
    return(tibble(freq, phase))
  }
)) %>% unnest(freq) %>%
  unnest(data) %>%
  group_by(worm_index, video_pref) %>%
  #filter(freq > 0.40 & freq < 0.65) |>
  mutate(relTime = realTime + (phase / (2*pi))*(2.22)) %>%
  mutate(flipped = case_when(
    any(relTime < 2.8 & relTime > 2.6 & curvature_mean_neck < 0) ~ "flipped",
    TRUE ~ "not flipped")) |>
  mutate(curvature_mean_neck = case_when(
    flipped == "flipped" ~ -curvature_mean_neck,
    TRUE ~ curvature_mean_neck
  )) |>
  mutate(curvature_mean_head = case_when(
    flipped == "flipped" ~ -curvature_mean_head,
    TRUE ~ curvature_mean_head
  ))

endpoints <- merged |>
  group_by(worm_index, date, video_pref, genotype, food) |>
  summarize(x_rotated = max(x_rotated),
            relTime = max(relTime),
            curvature_mean_neck = last(curvature_mean_neck),
            curvature_mean_head = last(curvature_mean_head),
            y_rotated = last(y_rotated))

# plot aligned curvature and endpoints
  merged |>
    #filter(genotype == "WT") |>
  ggplot(aes(x = relTime)) +
  geom_path(aes(group = worm_index, colour = food, y = curvature_mean_head), alpha = 0.5) +
  geom_point(data = endpoints, aes(y = curvature_mean_head), color = "black", size = 0.5) +
  geom_boxplot(data = endpoints, aes(y = 0.012, x = relTime), width = 0.001, outlier.shape = NA) +
  geom_smooth(aes(y = curvature_mean_head, group = food), method = "loess", span = 0.1) +
  #geom_density(data = endpoints, aes(y = after_stat(scaled)*0.01 + 0.012, x = relTime))
  scale_color_brewer(palette = "Set2") +
  facet_grid(food~genotype)

  # plot aligned x,y and endpoints
  merged |>
    ggplot(aes(x = x_rotated)) +
    geom_path(aes(group = worm_index, colour = food, y = y_rotated), alpha = 0.5) +
    facet_grid(food~genotype) +
    geom_point(data = endpoints, aes(y = y_rotated), color = "black", size = 0.5) +
    geom_boxplot(data = endpoints, aes(y = 500, x = x_rotated), width = 50, outlier.shape = NA) +
    #geom_smooth(aes(y = y_rotated, group = food), method = "loess", span = 0.1) +
    #geom_density(data = endpoints, aes(y = after_stat(scaled)*0.01 + 0.012, x = relTime))
    scale_color_brewer(palette = "Set2") +
    coord_cartesian(xlim = c(0,500))

  # plot aligned x,y and endpoints
  merged |>
    filter(genotype == "WT", food == "OP50") |>
    ggplot(aes(x = x_rotated)) +
    geom_path(aes(group = interaction(worm_index, video_pref), colour = food, y = y_rotated), alpha = 0.5) +
    #geom_point(data = endpoints, aes(y = y_rotated), color = "black", size = 0.5) +
    #geom_boxplot(data = endpoints, aes(y = 500, x = x_rotated), width = 50, outlier.shape = NA) +
    geom_smooth(aes(y = y_rotated, group = interaction(worm_index, video_pref)), method = "lm") +
    #geom_density(data = endpoints, aes(y = after_stat(scaled)*0.01 + 0.012, x = relTime))
    scale_color_brewer(palette = "Set2") +
    facet_wrap(~worm_index) +
    scale_x_continuous(limits = c(0,300))

align_reversals <- function(x, window = 500) {
  x %>%
    group_by(worm_index, video_pref) %>%
    nest() %>%
    mutate(fit = map(data, function(y) {
      dataset = y
      windowed = dataset[dataset$x_rotated <= window,]
      fit = lm(data = windowed, formula = y_rotated ~ x_rotated)
      intercept = fit$coefficients[[1]]
      theta = atan(fit$coefficients[[2]])
      tibble(intercept = intercept, theta = theta)})) %>%
    unnest(cols = c(data, fit)) %>%
    mutate(y_recentered = x_rotated*sin(-theta) + y_rotated*cos(-theta),
           x_recentered = x_rotated*cos(-theta) - y_rotated*sin(-theta)) %>%
    # make all initial head bends positive
    group_by(worm_index, video_pref) %>%
    mutate(y_recentered = case_when(
      mean(nth(y_recentered,4),nth(y_recentered,5),nth(y_recentered,6),na.rm = TRUE) < 0 ~ -y_recentered,
      TRUE ~ y_recentered))
}

align_reversals_Vnt <- function(x, window = 500) {
  x %>%
    group_by(worm_index, video_pref) %>%
    nest() %>%
    mutate(fit = map(data, function(y) {
      dataset = y
      windowed = dataset[dataset$x_rotated <= window,]
      fit = lm(data = windowed, formula = y_rotated ~ x_rotated)
      intercept = fit$coefficients[[1]]
      theta = atan(fit$coefficients[[2]])
      tibble(intercept = intercept, theta = theta)})) %>%
    unnest(cols = c(data, fit)) %>%
    mutate(y_recentered = x_rotated*sin(-theta) + y_rotated*cos(-theta),
           x_recentered = x_rotated*cos(-theta) - y_rotated*sin(-theta)) %>%
    # make all initial head bends positive
    group_by(worm_index, video_pref) %>%
    mutate(y_recentered = case_when(
      mean(nth(y_recentered,4),nth(y_recentered,5),nth(y_recentered,6),na.rm = TRUE) < 0 ~ -y_recentered,
      TRUE ~ y_recentered))
}

  endpoints %>% filter(genotype == "WT") %>%
    lm(data = ., relTime ~ food) %>%
    summary()

wormid <- 3914
2155
5674
3914
wormfit <- Touch1222 %>%
  filter(worm_index == wormid,
         realTime < 1.5) %>%
  lm(data = ., formula = curvature_mean_neck ~ sin(2*pi*.45*realTime) + cos(2*pi*.45*realTime))

newcurve <- tibble(realTime = seq(0.1,max(Touch1222$realTime)-.1, by = 0.1),
                   predicted = predict(wormfit, newdata = tibble(realTime)),
                   manFit =  wormfit$coefficients[2]*sin(2*pi*.45*realTime) +
                     wormfit$coefficients[3]*cos(2*pi*.45*realTime),
                   relTime = realTime + ((atan(wormfit$coefficients[3]/wormfit$coefficients[2])) / (2*pi))*2.22)


rbind(Touch1219,Touch1222,Touch1226,Touch1229) %>%
  #filter(worm_index == wormid) %>%
  filter(genotype == "WT", food == "tnaA") %>%
  ggplot() +
  geom_path(aes(x = x_rotated, y = y_rotated)) +
  geom_path(aes(x = x_body_rotated, y = y_body_rotated), colour = "red") +
  facet_wrap(~interaction(worm_index,video_pref)) +
  coord_cartesian(xlim = c(0,2000), ylim = c(-500,500))


#helper function to get midpoint from cut output
get_midpoint <- function(cut_label) {
  mean(as.numeric(unlist(strsplit(gsub("\\(|\\)|\\[|\\]", "", as.character(cut_label)), ","))), na.rm = TRUE)
}

HeadBodyAlign <- full_join(head,body) %>%
  group_by(worm_index, video_pref, x_bin) %>%
  mutate(x = get_midpoint(x_bin)) %>%
  group_by(worm_index, video_pref, genotype, food, x) %>%
  summarize(y_head = mean(y_rotated),
            y_body = mean(y_body_rotated))
  # need to first align the x coords from x_rotated (head) and x_body_rotated (midbody)
 HeadBodyAlign %>%
   filter(genotype == "WT", food == "OP50") %>%
  ggplot() +
  geom_path(aes(x = x, y = y_head)) +
  geom_path(aes(x = x, y = y_body), colour = "red") +
  #geom_path(aes(x = x_body_rotated, y = y_body_rotated), colour = "red") +
  facet_wrap(~worm_index) +
  coord_cartesian(xlim = c(0,2000), ylim = c(-500,500))

 HeadBodyAlign %>%
   filter(!is.na(y_body), !is.na(y_head)) %>%
   filter() %>%
   ggplot() +
   geom_path(aes(x = x, y = y_body-y_head, group = interaction(worm_index,video_pref), colour = food), alpha = 0.5) +
   #geom_path(aes(x = x_body_rotated, y = y_body_rotated), colour = "red") +
   facet_wrap(~worm_index) +
   facet_grid(food~genotype) +
   coord_cartesian(xlim = c(0,2000), ylim = c(-300,300))


######## reversals from Madhu: #########


merged <- align_reversals(
  rbind(
  Touch0205,
  Touch0126,
  #Touch1211,
                                Touch1210,
                                Touch1213,
                                Touch1218,
                                Touch1129,
                                Touch1220,
                                Touch1219,
                                Touch1222,
                                Touch1226,
                                Touch1229,
                                #Touch0116,
                                #Touch011650,
                                Touch0130)) #%>%
                       group_by(worm_index, video_pref) %>%
                       filter(max(realTime, na.rm = TRUE) > 1), window = 500) %>%
  mutate(date = substr(video_pref,1,8))

endpoints <- merged |>
  group_by(worm_index, date, video_pref, genotype, food) |>
  summarize(x_rotated = max(x_rotated),
            realTime = max(realTime),
            curvature_mean_neck = last(curvature_mean_neck),
            curvature_mean_head = last(curvature_mean_head),
            y_rotated = last(y_rotated),
            x_recentered = last(x_recentered))

timepoints <- merged |>
  group_by(worm_index, date, video_pref, genotype, food) |>
  filter(realTime == 1.26) %>% ungroup()

# head-body curvature
# p0 <- merged %>% filter(realTime > 0.05 & realTime < .5) %>%
#   ggplot(aes( x = curvature_mean_head, y = curvature_mean_neck)) +
#   geom_density_2d_filled(bins = 10) +
#   facet_grid(food~genotype) +
#   scale_color_viridis_c() +
#   coord_cartesian(xlim = c(-.01,0.01), ylim = c(-.01,0.01)) +
#   theme_classic() +
#   labs(title = "initial posture") +
#   theme(strip.background = element_blank(),
#         legend.position = "none",
#         axis.title.x = element_blank(),
#         axis.text.x = element_blank())
#
# p <- merged %>% filter(realTime > .5 & realTime < 1) %>%
#   ggplot(aes(x = curvature_mean_head, y = curvature_mean_neck)) +
#   geom_density_2d_filled(bins = 10) +
#   facet_grid(food~genotype) +
#   scale_color_viridis_c() +
#   coord_cartesian(xlim = c(-.01,0.01), ylim = c(-.01,0.01)) +
#   theme_classic() +
#   #theme(legend.position = "none") +
#   labs(title = ".25s < t < 0.75s") +
#   theme( legend.position = "none",
#     strip.background = element_blank(),
#     strip.text.x = element_blank()
#   )
#
#
# p2 <- merged %>% filter(realTime > 1 & realTime < 2.5) %>%
#   ggplot(aes( x = curvature_mean_head, y = curvature_mean_neck)) +
#   geom_density_2d_filled(bins = 10) +
#   facet_grid(food~genotype) +
#   scale_color_viridis_c() +
#   coord_cartesian(xlim = c(-.01,0.01), ylim = c(-.01,0.01)) +
#   theme_classic() +
#   theme(legend.position = "none",
#         axis.title.x = element_blank(),
#         axis.text.x = element_blank()) +
#   labs(title = ".75s < t < 2s") +
#   theme(
#     strip.background = element_blank()
#   )
#
# p3 <- merged %>% filter(realTime > 2.5 & realTime < 6) %>%
#   ggplot(aes( x = curvature_mean_head, y = curvature_mean_neck)) +
#   geom_density_2d_filled(bins = 10) +
#   facet_grid(food~genotype) +
#   scale_color_viridis_c() +
#   coord_cartesian(xlim = c(-.01,0.01), ylim = c(-.01,0.01)) +
#   theme_classic() +
#   theme(legend.position = "none") +
#   labs(title = "2s < t < 4s") +
#   theme(
#     strip.background = element_blank(),
#     strip.text.x = element_blank()
#   )
#
# p0/p | p2/p3

# p4 <- merged %>% filter(realTime > 4 & realTime < 6) %>%
#   ggplot(aes( x = abs(curvature_mean_head), y = abs(curvature_mean_neck))) +
#   geom_density_2d_filled() +
#   facet_grid(food~genotype) +
#   scale_color_viridis_c() +
#   coord_cartesian(xlim = c(0,0.01), ylim = c(0,0.01)) +
#   theme_classic() +
#   theme(legend.position = "none",
#         axis.title.x = element_blank(),
#         axis.text.x = element_blank()) +
#   labs(title = "3s < t < 6s") +
#   theme(
#     strip.background = element_blank(),
#     strip.text.x = element_blank()
#   )

# plotting distance covered:
path0 <- merged %>%
  ungroup() %>%
  mutate(strain = fct_relevel(strain, "N2", "PHX3928", "PHX7661", "TQ322")) %>%
  filter(#date == "02052024",
         realTime < 1.26,
         realTime > 0.2,
         #strain == "PHX7661",
         worm_index != "311"
         ) %>%
  ggplot(aes( x = x_body_rotated, y = realSpeed)) +
  geom_path(aes(group = interaction(worm_index, video_pref), colour = strain), alpha =0.2) +
  facet_grid(.~food) +
  scale_color_viridis_d(end = 0.9) +
  scale_fill_viridis_d(end = 0.9) +
  theme_classic() +
  geom_point(data = timepoints, aes(  x = x_body_rotated, y = realSpeed, color = strain), size = 0.5) +
  geom_smooth(aes(color = strain), method = "lm") +
  #geom_errorbarh(data = timepoints, aes(xmin = x_rotated, y = -300))
  geom_boxplot(data = mutate(timepoints, strain = fct_relevel(strain, "N2", "PHX3928", "PHX7661", "TQ322")),
               aes(fill = strain, x = x_body_rotated, y = -100),
               outlier.shape = NA,
               width = 100,
               alpha = 0.8) +
  geom_boxplot(aes(x = 1100,
                   y = realSpeed,
                   fill = strain),
               outlier.shape = NA,
               width = 100,
               alpha = 0.8) +
  guides(color = "none")

#
merged %>%
  ggplot(aes( x = x_recentered,
              y = abs(curvature_mean_head),
              group = interaction(worm_index, video_pref))) +
  geom_path(aes(colour = realTime), alpha = 0.5) +
  facet_grid(food~genotype) +
  scale_color_viridis_c() +
  theme_classic()

##### plottin
merged %>%
  ggplot(aes( x = realTime,
              y = d_relative_to_body_angular_velocity_head_tip)) +
  geom_path(aes(colour = realTime,
                group = interaction(worm_index, video_pref)), alpha = 0.25) +
  #geom_smooth()+
  #geom_point(data = endpoints, aes(y = curvature_mean_head), color = "black", size = 0.5) +
  #geom_boxplot(data = endpoints, aes(y = 100, x = realTime), width = 20, outlier.shape = NA) +
  facet_grid(food~genotype) +
  scale_color_viridis_c() +
  theme_classic()


path <- merged %>% filter(realTime > 0.25 & realTime < 0.75) %>%
  ggplot(aes( x = abs(curvature_mean_head), y = abs(curvature_mean_neck))) +
  geom_path(aes(alpha = 0.5)) +
  facet_grid(food~genotype) +
  scale_color_viridis_c() +
  theme_classic()

path2 <- merged %>% filter(realTime > 0.75 & realTime < 1.45) %>%
  ggplot(aes( x = abs(curvature_mean_head), y = abs(curvature_mean_neck))) +
  geom_path(aes(group = interaction(worm_index, video_pref), alpha = 0.5)) +
  facet_grid(food~genotype) +
  scale_color_viridis_c() +
  theme_classic()

path3 <- merged %>% filter(worm_index == "1772") %>%
  ggplot(aes( x = abs(curvature_mean_head),
              y = abs(curvature_mean_neck),
                      group = interaction(worm_index,video_pref))) +
  #geom_point(aes(color = realTime)) +
  geom_path(aes(group = interaction(worm_index, video_pref), colour = realTime)) +
  #geom_density_2d_filled() +
  facet_grid(food~genotype) +
  scale_color_viridis_c() +
  theme_classic()


library(gganimate)

anim <- path3 +
  labs(title = "{closest_state}") +
  transition_states(realTime, transition_length = 3, state_length = 1) +
  ease_aes('linear') +
  enter_fade() +
  exit_fade()

anim <- path3 +
  labs(title = "{closest_state}") +
  transition_reveal(realTime)


animate(anim, fps = 1)
