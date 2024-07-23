#' Filter and align worms that have been previously marked as good in Tierpsy Tracker
#'
#'
#' @param file this would be a metadata file for a folder to be analyzed or a
#' single hdf5 file if analyzing a single tracked video
#' @return a time-aligned
#' @export
#' @examples ExtractTierpsyTracks()
#'

ExtractTierpsyTracks <- function(file,
                               #genotype, #will incorporate this into metadata
                               #condition,
                               framerate = 10,
                               group = TRUE,
                               compare1 = strain,
                               compare2 = NULL,
                               use_only_metadata = FALSE,
                               #pixelRatio = 0.009703125, # pixels/mm if using zoom = 1 full res
                               pixelRatio = 1, # if tracked using pre-measured pixelratio
                               ...) {

  ####### file selection #####
  library(rhdf5)
  library(tidyverse)
  library(furrr)
  theme_set(theme_classic())
  if(group == TRUE) {
    metadata_path  = file.choose() |>
      dirname() |>
      fs::dir_ls(glob = "*metadata.csv")
    folder_path = dirname(metadata_path)
  } else { # this would be rare
    vid = vid
  }

  ### file checks ######
  if(basename(metadata_path) != "metadata.csv")
    stop("no metadata.csv in the selected folder")

  results_path <- fs::dir_ls(folder_path, glob = "*Results")

  if(length(results_path) == 0) {
    message("no Results folder in the path, have you analyzed more than one\n folder of videos with 'Batch Processing'?")
    message("if so, please select a file in a your Results folder")
    results_path = dirname(file.choose())
  }

  print(paste0("analyzing hdf5 results files in folder: \n",results_path))

  metadata <- read_csv(metadata_path, show_col_types = FALSE) |>
    mutate(video_pref = paste0(
      substr(
        fs::path_ext_remove(basename(filename)),
        nchar(fs::path_ext_remove(basename(filename)))-18,
        nchar(fs::path_ext_remove(basename(filename)))-5
      ))) # fix this for a nested results folder

  framerate <- unique(metadata$fps)

  message(paste0("Based on your metadata, analyzing ",
                 nrow(metadata),
                 " video files, \nwith ",
                 length(unique(metadata$strain)),
                 " strains and ",
                 length(unique(metadata$food)),
                 " food(s) at ",
                 unique(metadata$fps),
                 " Hz"))

  filenames <- fs::dir_ls(results_path, glob = "*featuresN.hdf5")

  if(use_only_metadata) {
    message("****using only videos identified in metadata file****")
    filenames <- filenames[str_detect(filenames,
                                      paste(metadata$video_pref, collapse = "|"))]
  } else {
    message(paste0("using all tracked videos, but note you have ",
                   length(filenames),
                   " tracked videos. \nopt for use_only_metadata = TRUE if you don't want this behavior"))
  }

  ##### loop through filtering and aligning worms #####
  plan(multisession, workers = 8)
  alldata <- future_map_dfr(filenames, function(dataset) {
    features = H5Fopen(dataset)

    ## all worms marked "1" are single worms
    good_worms <- features$trajectories_data %>%
      filter(worm_label == 0) %>%
      select(worm_index_joined, worm_label) %>%
      rename(worm_index = worm_index_joined) %>%
      group_by(worm_index, worm_label) %>%
      tally() %>%
      select(worm_index, worm_label)

    video_pref <- paste0(substr(basename(dataset),nchar(basename(dataset))-33,nchar(basename(dataset))-20))

    worm_data <- filter(features$timeseries_data,
                        worm_index %in% good_worms$worm_index) |>
      select(worm_index,
             timestamp,
             speed,
             coord_x_head,
             coord_y_head,
             motion_mode) %>%
             #path_curvature) %>%
      full_join(good_worms, by = join_by(worm_index)) %>%
      mutate(realSpeed = speed) |>
      group_by(worm_index) |>
      # keep worms with more than X frames
      filter(last(timestamp)-first(timestamp) > 100) |>
      mutate(rowNum = row_number(),
             realTime = rowNum / framerate) |>
      mutate(worm_index = as.factor(worm_index)) |>
      ungroup() |>
      mutate_if(is.integer, as.integer) |>
      mutate_if(is.double, as.double) |> # fix dbl[1d] list columns
      # align relative x and y of head
      group_by(worm_index) |>
      mutate(frame_gap = c(1,diff(timestamp))) |>
      # mark each change of motiom_mode (will keep only those with 0 value)
      mutate(state_change_tally = cumsum(replace_na(motion_mode != lag(motion_mode), 0)),
             state_change = replace_na(motion_mode != lag(motion_mode), 0),
             # replace the first state change so we can count the first run:
             state_change = replace(state_change, 1, 1),
             video_pref = video_pref) |>
      filter(!any(frame_gap > 3))
      # for merging with metadata



    h5closeAll()
    return(worm_data)

  }, .id = "datafile")

  extracted_tracks <- full_join(alldata, metadata, by = join_by(video_pref))

  tracks <- ggplot(extracted_tracks,
                   aes(x = coord_x_head,
                       y = coord_y_head)) +
    geom_path(aes(group = interaction(worm_index,
                                      genotype,
                                      video_pref),
                    color = motion_mode),
              alpha = 0.3) +
    facet_grid(rows = vars({{compare1}}),
               cols = vars({{compare2}})) +
    scale_fill_brewer(palette = "PuOr")

  ggsave(file.path(results_path,"tracks_plot.png"),
                  tracks,
                  width = 11,
                  height = 8.5)

  write_csv(extracted_tracks,
            file = file.path(results_path,
                             paste0(basename(results_path),"_extracted_tracks.csv")))

  foraging_data <- extracted_tracks |>
    select(genotype,
           strain,
           worm_index,
           video_pref,
           motion_mode,
           state_change,
           state_change_tally,
           timestamp,
           speed) |>
    group_by(genotype,strain,video_pref,worm_index) |>
    summarise(Tstart = first(timestamp) / framerate, # time in seconds
              Tend = last(timestamp) / framerate,
              time_bin_min = ceiling(Tstart / (5 * 60) + 0.01) * 5, # bins of 5 minutes each
              duration_frames = n(),
              n_rev_tally = sum(motion_mode == -1, na.rm = TRUE),
              n_fwd_tally = sum(motion_mode == 1, na.rm = TRUE),
              n_omega_tally = sum(motion_mode == 0, na.rm = TRUE),
              n_rev = sum(motion_mode == -1 & state_change == 1, na.rm = TRUE),
              n_fwd = sum(motion_mode == 1 & state_change == 1, na.rm = TRUE),
              n_omega = sum(motion_mode == 0 & state_change == 1, na.rm = TRUE),
              #framerate = framerate,
              rev_freq = (n_rev / duration_frames) * framerate * 60, # reversals per minute )
              omega_freq = (n_omega / duration_frames) * framerate * 60,
              rev_duration = (n_rev_tally / n_rev) / framerate, # reversal duration in seconds
              fwd_duration = (n_fwd_tally / n_fwd) / framerate) # fwd duration in seconds

  speed_data <- extracted_tracks |>
    group_by(genotype,strain,video_pref,worm_index,motion_mode) |>
    summarise(speed = mean(speed, na.rm = TRUE)) |>
    filter(!is.na(speed),
           motion_mode %in% c(-1,1)) %>%
    pivot_wider(names_from = motion_mode,
                values_from = speed) %>%
    rename( rev_speed = `-1`,
            fwd_speed = `1`)

  reversal_data <- extracted_tracks |>
    filter(motion_mode == -1) |>
    group_by(genotype,worm_index,video_pref,state_change_tally) |>
    # get total length of reversal
    summarise(rev_length = sqrt(
      (last(coord_x_head) - first(coord_x_head))^2 +
        (last(coord_y_head) - first(coord_y_head))^2
    ))
 ######

  foraging_data <- full_join(foraging_data,speed_data)

  rev_plot <-  ggplot(foraging_data,
                      aes(x = strain, y = rev_freq)) +
    geom_boxplot(aes(fill = strain)) +
    scale_fill_brewer(palette = "Paired") +
    facet_wrap(~time_bin_min)

  omega_plot <- ggplot(foraging_data,
                       aes(x = strain, y = omega_freq)) +
    geom_boxplot(aes(fill = strain)) +
    scale_fill_brewer(palette = "Paired") +
    facet_wrap(~time_bin_min)

  rev_time_plot <- ggplot(foraging_data,
                          aes(x = strain, y = rev_duration)) +
    geom_boxplot(aes(fill = strain), outlier.shape = NA) +
    scale_fill_brewer(palette = "Paired") +
    facet_wrap(~time_bin_min) +
    coord_cartesian(ylim = c(0,8))

  fwd_time_plot <- ggplot(foraging_data,
                          aes(x = strain, y = fwd_duration)) +
    geom_boxplot(aes(fill = strain), outlier.shape = NA) +
    scale_fill_brewer(palette = "Paired") +
    facet_wrap(~time_bin_min) +
    coord_cartesian(ylim = c(0,30))

  rev_speed_plot <- ggplot(foraging_data,
                           aes(x = strain, y = rev_speed)) +
    geom_boxplot(aes(fill = strain)) +
    scale_fill_brewer(palette = "Paired") +
    facet_wrap(~time_bin_min)

  fwd_speed_plot <- ggplot(foraging_data,
                           aes(x = strain, y = fwd_speed)) +
    geom_boxplot(aes(fill = strain)) +
    scale_fill_brewer(palette = "Paired") +
    facet_wrap(~time_bin_min)


  ## add in reversal lengths (will duplicate other rows so do this last)

  foraging_data <- full_join(foraging_data,reversal_data)

  rev_length_plot <- ggplot(foraging_data,
                            aes(x = strain, y = rev_length)) +
    geom_boxplot(aes(fill = strain)) +
    scale_fill_brewer(palette = "Paired") +
    facet_wrap(~time_bin_min)

  library(patchwork)

  p1 <- (rev_plot + omega_plot) / (rev_time_plot + fwd_time_plot)
  p2 <- (rev_speed_plot + fwd_speed_plot) / (rev_length_plot + plot_spacer())

  ggsave(file.path(results_path,"Foraging.png"),
         p1,
         width = 11,
         height = 8.5)

  ggsave(file.path(results_path,"RunSpeed_Lengths.png"),
         p2,
         width = 11,
         height = 8.5)

  write_csv(foraging_data,
            file = file.path(results_path,
                             paste0(basename(results_path),"_foraging_data.csv")))

  return(extracted_tracks)
}


