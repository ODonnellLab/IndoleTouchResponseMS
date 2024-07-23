library(tidyverse)
touch0220 <- read_csv(file.choose())

magazine.colours <- function(n, set=NULL) {
  set <- match.arg(set, c('1', '2'))
  palette <- c("red4", "darkslategray3", "dodgerblue1", "darkcyan",
               "gray79", "black", "skyblue2", "dodgerblue4",
               "purple4", "maroon", "chocolate1", "bisque3", "bisque",
               "seagreen4", "lightgreen", "skyblue4", "mediumpurple3",
               "palevioletred1", "lightsalmon4", "darkgoldenrod1")
  if (set == 2)
    palette <- rev(palette)
  if (n > length(palette))
    warning('generated palette has duplicated colours')
  rep(palette, length.out=n)
}

theme_black = function(base_size = 12, base_family = "") {

  theme_grey(base_size = base_size, base_family = base_family) %+replace%

    theme(
      # Specify axis options
      axis.line.x = element_line(color = "white", size  =  0.2),
      axis.line.y = element_line(color = "white", size  =  0.2),
      axis.text.x = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),
      axis.text.y = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),
      axis.ticks = element_line(color = "white", size  =  0.2),
      axis.title.x = element_text(size = base_size, color = "white", margin = margin(0, 10, 0, 0)),
      axis.title.y = element_text(size = base_size, color = "white", angle = 90, margin = margin(0, 10, 0, 0)),
      axis.ticks.length = unit(0.3, "lines"),
      # Specify legend options
      legend.background = element_rect(color = NA, fill = "black"),
      legend.key = element_rect(color = "white",  fill = "black"),
      legend.key.size = unit(1.2, "lines"),
      legend.key.height = NULL,
      legend.key.width = NULL,
      legend.text = element_text(size = base_size*0.8, color = "white"),
      legend.title = element_text(size = base_size*0.8, face = "bold", hjust = 0, color = "white"),
      legend.position = "right",
      legend.text.align = NULL,
      legend.title.align = NULL,
      legend.direction = "vertical",
      legend.box = NULL,
      # Specify panel options
      panel.border = element_blank(),
      panel.background = element_blank(),
      #panel.background = element_rect(fill = "black", color  =  NA),
      #panel.border = element_rect(fill = NA, color = "white"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.spacing = unit(0.5, "lines"),
      # Specify facetting options
      strip.background = element_rect(fill = "grey30", color = "grey10"),
      strip.text.x = element_text(size = base_size*0.8, color = "white"),
      strip.text.y = element_text(size = base_size*0.8, color = "white",angle = -90),
      # Specify plot options
      plot.background = element_rect(color = "black", fill = "black"),
      plot.title = element_text(size = base_size*1.2, color = "white"))
      #plot.spacing = unit(rep(1, 4), "lines"))
}

touch0220 %>%
  filter(worm_index == 9428)

touch0220 %>%
  group_by(video_pref, worm_index, genotype) %>%
  tally() %>%
  view()

worms <- H5Fopen(file.choose())

wormplot <- worms$timeseries_data %>%
  filter(worm_index == 9428)

wormplot <- wormplot %>%
  filter(!is.na(speed)) %>%
  mutate(relTime = (timestamp - as.numeric(first(timestamp))) / 50) %>%
  filter(relTime < 11) %>%
  mutate_if(is.double, as.double)


max(wormplot$speed, na.rm = TRUE)

p1 <- ggplot(wormplot, aes(x = relTime, y = speed)) +
  geom_path(colour = "grey") +
  theme_black() +
  theme(axis.title.x = element_blank())+
  guides(color = "none",
         x = "none") +
  coord_cartesian(ylim = c(-450, 0)) +
  labs(y = "speed")

p2 <- ggplot(wormplot, aes(x = relTime, y = curvature_neck)) +
  geom_path(aes(colour = curvature_neck)) +
  scale_color_viridis_c() +
  theme_black() +
  theme(axis.title.x = element_blank())+
  guides(color = "none",
         x = "none") +
  labs(y = "neck \ncurvature")

p3 <- ggplot(wormplot, aes(x = relTime, y = angular_velocity)) +
  geom_path(aes(colour = angular_velocity)) +
  scale_color_viridis_c(option = "plasma") +
  theme_black() +
  guides(color = "none") +
  labs(y = "angular \nvelocity",
       x = "time (s)")

library(patchwork)
plot <- p1 / p2 / p3

plot + plot_layout(heights = c(1,1,1)) &
  theme(plot.margin = unit(c(0,0,0,0), "cm"))


library(gganimate)
anim_1 <- p1 +
  transition_reveal(relTime)
animate(anim_1, height = 100, width =400,
        fps = 16)
anim_save("speed.gif")
anim_2 <- p2 +
  transition_reveal(relTime)
animate(anim_2, height = 100, width =400,
        fps = 16)
anim_save("neck.gif")
anim_3 <- p3 +
  transition_reveal(relTime)
animate(anim_3, height = 100, width =400,
        fps = 16)
anim_save("angvel.gif")


