#' Filter and align worms that have been previously marked as good in Tierpsy Tracker
#'
#'
#' @param file this would be a metadata file for a folder to be analyzed or a
#' single hdf5 file if analyzing a single tracked video
#' @return a time-aligned
#' @export
#' @examples ForagingAnalysis()
#'

ForagingAnalysis <- function(file,
                             # genotype, #will incorporate this into metadata
                             # condition,
                             framerate = 10,
                             min_frames = 50, # minimum number of frames to retain a work track
                             group = TRUE,
                             compare1 = strain,
                             time_bin = 1, # time bin for state probability calculation
                             compare2 = NULL,
                             use_only_metadata = FALSE,
                             # pixelRatio = 0.009703125, # pixels/mm if using zoom = 1 full res
                             pixelRatio = 1, # if tracked using pre-measured pixelratio
                             ExtractTierpsyData = FALSE, # if TRUE, run ExtractTierpsyTracks first
                             ExportPlots = FALSE,
                             ...) {
  ####### file selection #####
  library(rhdf5)
  library(tidyverse)
  library(furrr)
  theme_set(theme_classic())
  if (group == TRUE) {
    metadata_path <- file.choose() |>
      dirname() |>
      fs::dir_ls(glob = "*metadata.csv")
    folder_path <- dirname(metadata_path)
  } else { # this would be rare
    vid <- vid
  }

  #### file checks ######
  if (basename(metadata_path) != "metadata.csv") {
    stop("no metadata.csv in the selected folder")
  }

  results_path <- fs::dir_ls(folder_path, glob = "*Results")

  if (length(results_path) == 0) {
    message("no Results folder in the path, have you analyzed more than one\n folder of videos with 'Batch Processing'?")
    message("if so, please select a file in a your Results folder")
    results_path <- dirname(file.choose())
  }

  print(paste0("analyzing hdf5 results files in folder: \n", results_path))

  metadata <- read_csv(metadata_path, show_col_types = FALSE) |>
    mutate(video_pref = paste0(
      substr(
        fs::path_ext_remove(basename(filename)),
        nchar(fs::path_ext_remove(basename(filename))) - 18,
        nchar(fs::path_ext_remove(basename(filename))) - 5
      )
    )) # fix this for a nested results folder

  framerate <- unique(metadata$fps)

  message(paste0(
    "Based on your metadata, analyzing ",
    nrow(metadata),
    " video files, \nwith ",
    length(unique(metadata$strain)),
    " strains and ",
    length(unique(metadata$food)),
    " food(s) at ",
    unique(metadata$fps),
    " Hz"
  ))

  filenames <- fs::dir_ls(results_path, glob = "*featuresN.hdf5")

  if (use_only_metadata) {
    message("****using only videos identified in metadata file****")
    filenames <- filenames[str_detect(
      filenames,
      paste(metadata$video_pref, collapse = "|")
    )]
  } else {
    message(paste0(
      "using all tracked videos, but note you have ",
      length(filenames),
      " tracked videos. \nopt for use_only_metadata = TRUE if you don't want this behavior"
    ))
  }


  #### Analysis ####
if(ExtractTierpsyData) {
  ##### loop through filtering and aligning worms #####
  plan(multisession, workers = 8)
  alldata <- future_map_dfr(filenames, function(dataset) {
    features <- H5Fopen(dataset)

    ## all worms marked "1" are annotated single worms:
    ## 0 = undefined = grey, not annotated
    ## 1 = single worms, green, annotated
    ## 2 = worm cluster, blue, annotaed
    ## 3 = bad worm, red, annotated
    good_worms <- features$trajectories_data %>%
      filter(!worm_label %in% c(2,3)) %>%
      select(worm_index_joined, worm_label) %>%
      rename(worm_index = worm_index_joined) %>%
      group_by(worm_index, worm_label) %>%
      tally() %>%
      select(worm_index, worm_label)

    video_pref <- paste0(substr(basename(dataset), nchar(basename(dataset)) - 33, nchar(basename(dataset)) - 20))

    worm_data <- filter(
      features$timeseries_data,
      worm_index %in% good_worms$worm_index
    ) |>
      select(
        worm_index,
        timestamp,
        speed,
        coord_x_head,
        coord_y_head,
        motion_mode,
        turn
      ) %>%
      full_join(good_worms, by = join_by(worm_index)) %>%
      mutate(realSpeed = speed) |>
      group_by(worm_index) |>
      # keep worms with more than X frames
      filter(last(timestamp) - first(timestamp) > min_frames) |>
      mutate(
        rowNum = row_number(),
        relTime = rowNum / framerate,
        realTime = timestamp / framerate
      ) |>
      mutate(worm_index = as.factor(worm_index)) |>
      ungroup() |>
      mutate_if(is.integer, as.integer) |>
      mutate_if(is.double, as.double) |> # fix dbl[1d] list columns
      # align relative x and y of head
      group_by(worm_index) |>
      mutate(frame_gap = c(1, diff(timestamp))) |>
      # mark each change of motiom_mode (will keep only those with 0 value)
      mutate(
        state_change_tally = cumsum(replace_na(motion_mode != lag(motion_mode), 0)),
        state_change = replace_na(motion_mode != lag(motion_mode), 0),
        # replace the first state change so we can count the first run:
        state_change = replace(state_change, 1, 1),
        video_pref = video_pref
      ) |>
      filter(!any(frame_gap > 3))
    # for merging with metadata
    #


    h5closeAll()
    return(worm_data)
  }, .id = "datafile")

  extracted_tracks <- full_join(alldata, metadata, by = join_by(video_pref))

  write_csv(extracted_tracks,
            file = file.path(
              results_path,
              paste0(basename(results_path), "_extracted_tracks.csv")
            )
  )
} else {
  extracted_tracks <- read_csv(
    file = file.path(results_path,
                     paste0(basename(results_path),
                            "_extracted_tracks.csv")
  ))
}

  ## need to normalize zeros by the track duration somehow

  n_turns <- extracted_tracks |>
    select(
      genotype,
      strain,
      worm_index,
      video_pref,
      motion_mode,
      turn,
      state_change,
      state_change_tally,
      realTime
    ) %>%
    group_by(
      genotype,
      strain,
      worm_index,
      video_pref
    ) %>%
    mutate(time_bin_min = ceiling(realTime / (time_bin * 60) + 0.01) * time_bin,  # make a time bin window (5min)
      #time_bin_min = ceiling(first(realTime) / (time_bin * 60) + 0.01) * time_bin,
      turn_init = replace_na(turn == 1 & turn != lag(turn), 0),
      rev_init = replace_na(motion_mode == -1 & motion_mode != lag(motion_mode), 0)) %>%
    ungroup() %>%
    group_by(
      genotype,
      strain,
      worm_index,
      video_pref,
      time_bin_min
    ) %>%
    summarize(
      n_worm_frames = n(),
      n_worms = n_worm_frames / (time_bin * 60 * framerate), # number of worms tracked in the interval, normalized to number of frames
      n_turns = sum(turn_init),
      n_revs = sum(rev_init),
      worm_turn_freq = ( n_turns / n_worms ) / time_bin, # turn frequency in minutes
      worm_rev_freq = ( n_revs / n_worms ) / time_bin
    ) %>%
    ungroup() %>%
    group_by(genotype,
             strain,
             video_pref,
             time_bin_min) %>%
    summarise(worms_in_bin = sum(n_worms),
           avg_turn_freq = (sum(n_turns) / worms_in_bin) / time_bin,
           avg_rev_freq = (sum(n_revs) / worms_in_bin) / time_bin)

  turn_plot <- n_turns %>%
    pivot_longer(cols = 6:7, names_to = "category", values_to = "frequency_min") %>%
    arrange(strain,category,time_bin_min) %>%
    ggplot(aes(x = time_bin_min)) +
    geom_path(aes(y = frequency_min,
                  group = interaction(strain,category)
                  , color = strain,
                  alpha = category)) +
    scale_color_brewer(palette = "Set2") +
    scale_alpha_manual(values = c(0.5,1))

  ggsave(file.path(results_path, "Turn_freq.png"),
         turn_plot,
         width = 11,
         height = 8.5)


    # filter(turn == 1 & state_change == 1) %>%
    # summarize(turns = sum(turn, na.rm = TRUE))
  #
  # track_durations <- extracted_tracks |>
  #   select(
  #     genotype,
  #     strain,
  #     worm_index,
  #     video_pref,
  #     timestamp,
  #     relTime,
  #     realTime
  #   ) %>%
  #   group_by(
  #     genotype,
  #     strain,
  #     worm_index,
  #     video_pref
  #   ) %>%
  #   mutate(time_bin_min = ceiling(first(realTime) / (time_bin * 60) + 0.01) * time_bin) %>% # make a time bin window (5min)
  #   summarise(
  #     duration_frames = last(timestamp) - first(timestamp),
  #     duration_time = last(relTime) - first(relTime),
  #     time_bin_min = first(time_bin_min)
  #   )
  #
  # n_turns <- full_join(n_turns, track_durations) %>%
  #   mutate(turns = case_when(
  #     is.na(turns) ~ 0,
  #     TRUE ~ turns
  #   )) %>%
  #   mutate(turn_freq_min = turns / (duration_time / 60))

  ### save plot
  write_csv(n_turns,
    file = file.path(
      results_path,
      paste0(basename(results_path), "_turn_data.csv")
    )
  )



  if(ExportPlots) {
    foraging_data <- extracted_tracks |>
      select(
        genotype,
        strain,
        worm_index,
        video_pref,
        motion_mode,
        state_change,
        state_change_tally,
        timestamp,
        relTime,
        realTime,
        speed
      ) |>
      group_by(genotype,
               strain,
               video_pref,
               worm_index) |>
      mutate(time_bin_min = ceiling(realTime / (time_bin * 60) + 0.01) * time_bin,
             rev_init = replace_na(motion_mode -1 & motion_mode != lag(motion_mode), 0)) |>



      summarise(
        Tstart = first(realTime), # time in seconds
        Tend = last(realTime),
        time_bin_min = ceiling(Tstart / (time_bin * 60) + 0.01) * time_bin, # bins of 5 minutes each, start of track is
        duration_frames = n(),
        n_rev_tally = sum(motion_mode == -1, na.rm = TRUE),
        n_fwd_tally = sum(motion_mode == 1, na.rm = TRUE),
        n_omega_tally = sum(motion_mode == 0, na.rm = TRUE), # probably this is pause, bot omega
        n_rev = sum(motion_mode == -1 & state_change == 1, na.rm = TRUE),
        n_fwd = sum(motion_mode == 1 & state_change == 1, na.rm = TRUE),
        n_omega = sum(motion_mode == 0 & state_change == 1, na.rm = TRUE),
        # framerate = framerate,
        rev_freq = (n_rev / duration_frames) * framerate * 60, # reversals per minute )
        omega_freq = (n_omega / duration_frames) * framerate * 60,
        rev_duration = (n_rev_tally / n_rev) / framerate, # reversal duration in seconds
        fwd_duration = (n_fwd_tally / n_fwd) / framerate
      ) # fwd duration in seconds


    speed_data <- extracted_tracks |>
      group_by(genotype, strain, video_pref, worm_index, motion_mode) |>
      summarise(speed = mean(speed, na.rm = TRUE)) |>
      filter(
        !is.na(speed),
        motion_mode %in% c(-1, 1)
      ) %>%
      pivot_wider(
        names_from = motion_mode,
        values_from = speed
      ) %>%
      rename(
        rev_speed = `-1`,
        fwd_speed = `1`
      )

    reversal_data <- extracted_tracks |>
      filter(motion_mode == -1) |>
      group_by(genotype, worm_index, video_pref, state_change_tally) |>
      # get total length of reversal
      summarise(rev_length = sqrt(
        (last(coord_x_head) - first(coord_x_head))^2 +
          (last(coord_y_head) - first(coord_y_head))^2
      ))

    foraging_data <- full_join(foraging_data, speed_data)

    rev_plot <- ggplot(
      foraging_data,
      aes(x = strain, y = rev_freq)
    ) +
      geom_boxplot(aes(fill = strain)) +
      scale_fill_brewer(palette = "Paired") +
      facet_wrap(~time_bin_min)

    omega_plot <- ggplot(
      foraging_data,
      aes(x = strain, y = omega_freq)
    ) +
      geom_boxplot(aes(fill = strain)) +
      scale_fill_brewer(palette = "Paired") +
      facet_wrap(~time_bin_min)

    rev_time_plot <- ggplot(
      foraging_data,
      aes(x = strain, y = rev_duration)
    ) +
      geom_boxplot(aes(fill = strain), outlier.shape = NA) +
      scale_fill_brewer(palette = "Paired") +
      facet_wrap(~time_bin_min) +
      coord_cartesian(ylim = c(0, 8))

    fwd_time_plot <- ggplot(
      foraging_data,
      aes(x = strain, y = fwd_duration)
    ) +
      geom_boxplot(aes(fill = strain), outlier.shape = NA) +
      scale_fill_brewer(palette = "Paired") +
      facet_wrap(~time_bin_min) +
      coord_cartesian(ylim = c(0, 30))

    rev_speed_plot <- ggplot(
      foraging_data,
      aes(x = strain, y = rev_speed)
    ) +
      geom_boxplot(aes(fill = strain)) +
      scale_fill_brewer(palette = "Paired") +
      facet_wrap(~time_bin_min)

    fwd_speed_plot <- ggplot(
      foraging_data,
      aes(x = strain, y = fwd_speed)
    ) +
      geom_boxplot(aes(fill = strain)) +
      scale_fill_brewer(palette = "Paired") +
      facet_wrap(~time_bin_min)


    ## add in reversal lengths (will duplicate other rows so do this last)

    foraging_data <- full_join(foraging_data, reversal_data)

    rev_length_plot <- ggplot(
      foraging_data,
      aes(x = strain, y = rev_length)
    ) +
      geom_boxplot(aes(fill = strain)) +
      scale_fill_brewer(palette = "Paired") +
      facet_wrap(~time_bin_min)

    library(patchwork)

    p1 <- (rev_plot + omega_plot) / (rev_time_plot + fwd_time_plot)
    p2 <- (rev_speed_plot + fwd_speed_plot) / (rev_length_plot + plot_spacer())

    ggsave(file.path(results_path, "Foraging.png"),
           p1,
           width = 11,
           height = 8.5
    )

    ggsave(file.path(results_path, "RunSpeed_Lengths.png"),
           p2,
           width = 11,
           height = 8.5
    )

    write_csv(foraging_data,
              file = file.path(
                results_path,
                paste0(basename(results_path), "_foraging_data.csv")
              )
    )

    return(extracted_tracks)
  } else {
    return(extracted_tracks)
  }

}
