# library(tidyverse)
# library(zoo)
# library(scales)
#
# #####
# cutoff <- 0.3
#
# Allworms <- read_csv('~/Desktop/2023-10-28_manualtracking.csv') %>%
#   mutate(worm = cumsum(time - lag(time, default = 0) > 10 | time - lag(time, default = 0) < 0 | abs(x - lag(x, default = 0)) > 20)) %>%
#   filter(!(worm == 7 & time > 2140)) %>%
#   group_by(worm) %>%
#   mutate(relTime = time - first(time),
#          relX = x-first(x), # will be 0 for first point
#          relY = y-first(y),
#          relX_end = last(relX),
#          relY_end = last(relY),
#          angle = atan2(relY_end, relX_end),
#          # rotated to start at 0,0 and backward movement is to the right
#          x_rotated = relX*cos(-angle) - relY*sin(-angle),
#          y_rotated = relX*sin(-angle) + relY*cos(-angle),
#          y_fft = itsmr::smooth.fft(y_rotated, cutoff),
#          y_fft_0 = y_fft - first(y_fft))
#
# # get rid of really short tracks:
# short_tracks <- Allworms %>%
#   group_by(worm) %>%
#   summarise(length = max(relTime)) %>%
#   filter(length < 20)
#
# # first impute missing y values:
#
# Allworms <- Allworms %>%
#   ungroup() %>%
#   expand(worm, relTime) %>% # add in missing time points
#   full_join(Allworms) %>%
#   filter(!worm  %in% short_tracks$worm) %>%
#   fill(genotype, .direction = "down") %>%
#   mutate(wormID = interaction(genotype,worm)) %>%
#   group_by(worm) %>%
#   mutate(y_rotated_pad = zoo::na.approx(y_rotated, na.rm = FALSE),
#          y_fit = zoo::na.approx(y_fft_0, na.rm = FALSE),
#          x_rotated = zoo::na.approx(x_rotated, na.rm = FALSE)) %>%
#   filter(!is.na(y_fit))
#
#
#   # some smoothing, maybe won't use this
# Allworms <- Allworms |> mutate(y_rotated_mean = rollmean(y_rotated, k=5, fill = 0),
#          delY = case_when(
#            is.na(y_fft - lag(y_fft, 1)) ~ 0,
#            TRUE ~ y_fft - lag(y_fft, 1)),
#          meandelY = rollmean(delY, k = 3, fill = 0))
#
# # flip the head bend to positive for all trajectories
# p<- Allworms %>%
#   ungroup %>%
#   mutate(genotype = fct_relevel(genotype, "N2")) %>%
#   group_by(worm) %>%
#   mutate(y_fit = case_when(
#     nth(y_fit, 2) - nth(y_fit, 3) < 0 ~ -y_fit,
#     TRUE ~ y_fit)) %>%
#   ggplot(aes(x = relTime/10, y = y_fit)) +
#   geom_line(aes(group = worm, color = genotype), alpha = 0.75) +
#   scale_color_manual(values = c("darkgrey","darkgreen")) +
#   labs(x = "time (s)", y = "head deflection (mm)") +
#   # theme_black() +
#   theme_classic()
#
# #need to filter out the low frequency data (translational locomotion) to focus on head swings only
#
# # function for any number of harmonics
# nff <- function(x = NULL, n = NULL, up = 10L, plot = TRUE, add = FALSE, main = NULL, ...) {
#   dff <- fft(x)
#   t <- seq(1, length(x))
#   # upsample time
#   nt <- seq(from = 1, to = length(x) + 1 - 1 / up, by = 1 / up)
#   ndff <- array(data = 0, dim = c(length(nt), 1L))
#   ndff[1] <- dff[1] # Always, it's the DC component
#   if(n != 0) {
#     ndff[2:(n + 1)] <- dff[2:(n + 1)] # The positive frequencies always come first
#     # The negative ones are trickier
#     ndff[length(ndff):(length(ndff) - n + 1)] <- dff[length(x):(length(x) - n + 1)]
#   }
#   #The inverses
#   indff = fft(ndff/length(x), inverse = TRUE)
#   idff = fft(dff/length(x), inverse = TRUE)
#   if(plot){
#     if(!add){
#       plot(x = t, y = x, pch = 16L, xlab = "Time", ylab = "Measurement",
#            main = ifelse(is.null(main), paste(n, "harmonics"), main))
#       lines(y = Mod(idff), x = t, col = adjustcolor(1L, alpha = 0.5))
#     }
#     lines(y = Mod(indff), x = nt, ...)
#   }
#   ret = data.frame(time = nt, y = Mod(indff))
#   return(ret)
# }
#
# # Simplified function for 1st harmonic normalization
# FirstHarmNormalize <- function(x, Ypad = 200) {
#   # To move all Y in the positive range, add a constant
#   dff <- fft(x + Ypad)
#   ndff <- array(data = 0, dim = c(length(x), 1))
#   ndff[1] <- dff[1]
# # first positive frequency:
#   ndff[2] <- dff[2] # The positive frequencies always come first
# # First negative frequency, all the rest are 0 for 1st harmonic
#   ndff[length(ndff)] <- dff[length(x)]
#   indff = fft(ndff/length(x), inverse = TRUE)
#   YHarm = Mod(indff)
#   Normed <- (x + Ypad) - YHarm
#   return(Normed[,1])
# }
#
# Filtered <- Allworms %>%
#   group_by(worm) %>%
#   nest() %>%
#   # high pass filter
#   mutate(YHarm1 = map(data, function(df) {
#   FirstHarmNormalize(df$y_rotated_pad)
#     })) %>%
#   unnest(cols = c(data, YHarm1)) %>%
#   select(genotype, worm, wormID, relTime/10, x_rotated, YHarm1) %>%
#   rename(y_fit = YHarm1) %>%
#   group_by(worm) %>%
#   # low pass filter
#   mutate(y_fft = itsmr::smooth.fft(y_fit, cutoff),
#          y_centered = y_fft - first(y_fft))
#
# # now flip those that have negative head swing orientation to start:
# NegSwing <- Filtered %>%
#   filter(relTime < 15) %>%
#   group_by(worm) %>%
#   summarize(min = min(y_centered),
#             max = max(y_centered)) %>%
#   filter(abs(min) > max)
#
# Filtered <- Filtered %>% mutate(y_centered = case_when(
#   worm %in% NegSwing$worm ~ -y_centered,
#   TRUE ~ y_centered
# ))
#
# # reordering for heatmap:
# order <- Filtered %>%
#   filter(relTime < 15) %>%
#   group_by(worm) %>%
#   summarize(min = min(y_fft),
#             max = max(y_fft),
#             position = which.min(y_fft))
#
# Filtered %>%
#   mutate(genotype = fct_relevel(genotype, "N2")) %>%
#   ggplot(aes(x = relTime/10, y = y_centered)) +
#   geom_line(aes(group = worm, color = genotype),
#             alpha = 0.5,
#             linewidth = 1) +
#   scale_color_manual(values = c("orange","darkgreen")) +
#   labs(x = "time (s)", y = "head deflection (um)") +
#   #theme_black() #+
#   theme_classic() +
#   facet_grid(~genotype)
#
# # get amplitudes of head swings:
# Filtered %>%
#   group_by(genotype, worm) %>%
#   summarize(amplitude = max(y_centered) - min(y_centered)) %>%
#   lm(formula = amplitude~ genotype) %>%
#   summary()
#   ggplot(aes(x = genotype, y = amplitude)) +
#   geom_boxplot()
#
# # distribution of reversal durations:
# Filtered %>%
#   mutate(genotype = fct_relevel(genotype, "N2")) %>%
#   group_by(genotype, worm) %>%
#   summarize(time = max(relTime)) %>%
#   lm(formula = time ~ genotype) %>%
#   summary()
# ggplot(aes(x = genotype, y = time)) +
#   geom_boxplot()
#
#
# ### heatmap
#
# (p4 <- Filtered %>%
#   full_join(.,order) %>%
#   mutate(genotype = fct_relevel(genotype, "N2")) %>%
#   ggplot(aes(x = relTime/10, y = fct_reorder(wormID, position))) +
#   geom_tile(aes(fill = y_fft)) +
#   scale_fill_viridis_c(limits = c(-30,30),
#                        na.value = 0,
#                        oob=squish,
#                        option = "magma") +
#   facet_grid(genotype~., scales = "free") +
#   theme_grey())
#
#
# # heat map of position:
# #
# xbins <- Filtered %>%
#   full_join(.,order) %>%
#   ungroup() %>%
#   mutate(x_bin = cut_width(x_rotated, width = 7.5)) %>%
#   group_by(genotype, wormID, position) %>%
#   expand(x_bin)
#
#
# p5 <- Filtered %>%
#   full_join(.,order) %>%
#   ungroup() %>%
#   mutate(x_bin = cut_width(x_rotated, width = 7.5)) %>%
#   group_by(genotype, wormID, position, x_bin) %>%
#   summarize(y_fft = mean(y_fft)) %>%
#   group_by(wormID) %>%
#   full_join(.,xbins) %>%
#   group_by(genotype, wormID, x_bin, position) %>%
#   nest() %>%
#   arrange(wormID,x_bin) %>%
#   unnest(cols = c(data)) %>%
#   group_by(wormID) %>%
#   #mutate(y_imputed = zoo::na.locf(y_fft)) %>%
#     ggplot(aes(x = x_bin, y = fct_reorder(wormID, position))) +
#     geom_tile(aes(fill = y_fft)) +
#     scale_fill_viridis_c(limits = c(-30,30),
#                          na.value = 0,
#                          oob=squish,
#                          option = "magma") +
#     facet_grid(genotype~., scales = "free_y") +
#     theme_grey()
#
#
#
# p6 <- Filtered %>%
#   full_join(.,order) %>%
#   ungroup() %>%
#   mutate(x_bin = cut_width(x_rotated, width = 7.5)) %>%
#   group_by(genotype, wormID, position, x_bin) %>%
#   summarize(y_fft = mean(y_fft)) %>%
#   group_by(wormID) %>%
#   full_join(.,xbins) %>%
#   group_by(genotype, wormID, x_bin, position) %>%
#   nest() %>%
#   arrange(wormID,x_bin) %>%
#   unnest(cols = c(data)) %>%
#   group_by(wormID) %>%
#   mutate(genotype = fct_relevel(genotype, "N2")) %>%
#   ggplot(aes(x = x_bin, y = y_fft)) +
#   geom_line(aes(group = worm, color = genotype),
#             alpha = 0.5,
#             linewidth = 1) +
#   scale_color_manual(values = c("orange","darkgreen")) +
#   labs(x = "time (s)", y = "head deflection (um)") +
#   #theme_black() #+
#   theme_classic() +
#   facet_grid(~genotype)
#
# y <- Allworms %>%
#   filter(worm == 9) %>%
#   pull(y_fit)
#
# png("res_18.png")
# # this will give the first harmonic
# res = nff(x = y, n = 10L, up = 1L, col = 3L)
# dev.off()
# #
# # sum5to18 = nff(x = y, n = 18L, up = 10L, plot = FALSE)
# # sum5to18$y = sum5to18$y - nff(x = y, n = 4L, up = 10L, plot = FALSE)$y
# # png("sum5to18.png")
# # plot(sum5to18, pch = 16L, xlab = "Time", ylab = "Measurement", main = "5th to 18th harmonics sum", type = "l", col = 2)
# # dev.off()
# #
# colors = rainbow(36L, alpha = 0.3)
# nff(x = y, n = 10L, up = 10L, col = colors[1])
# png("all_waves.png")
# for(i in 1:6){
#   ad = ifelse(i == 1, FALSE, TRUE)
#   nff(x = y, n = i, up = 10L, col = colors[i], add = ad, main = "All waves up to 18th harmonic")
# }
# dev.off()
#
#
#
# p2 <- Allworms %>%
#   ungroup %>%
#   mutate(genotype = fct_relevel(genotype, "N2")) %>%
#   filter(x_rotated > 0,
#          !worm %in% c(9,17)) %>%
#   mutate(y_fft_0 = case_when(
#     nth(y_fft_0, 2) - nth(y_fft_0, 3) < 0 ~ -y_fft_0,
#     TRUE ~ y_fft)) %>%
#   #filter(!(worm == 7 & time > 2140)) %>% # weird discontinuity with N2, #7
#   group_by(worm) %>%
#   ggplot(aes(x = relTime, y = y_fft_0)) +
#   #geom_line(aes(group = worm, color = time))
#   geom_smooth(aes(group = genotype, color = genotype),
#               method = "loess",
#               span = 0.25,
#               se = FALSE,
#               linewidth = 0.75) +
#   scale_color_manual(values = c("darkgrey","darkgreen")) +
#   labs(x = "distance (mm)", y = "head deflection (mm)") +
#   # theme_black() +
#   # theme(axis.line.x = element_blank(),
#   #       axis.title.x = element_blank(),
#   #       axis.ticks.x = element_blank(),
#   #       axis.text.x = element_blank(),
#   #       axis.title.y = element_blank())
#   theme_classic()
#
#
#
# library(patchwork)
# p2/p + plot_layout(heights = c(0.25, 1))
