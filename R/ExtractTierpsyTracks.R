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

  ### file checks ######
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

}


