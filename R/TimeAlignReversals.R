#' Filter and align worms that have been previously marked as good in Tierpsy Tracker
#'
#'
#' @param file this would be a metadata file for a folder to be analyzed or a
#' single hdf5 file if analyzing a single tracked video
#' @return a time-aligned
#' @export
#' @examples TimeAlignReversals(genotype = "N2", condition = "off-food")
#'

TimeAlignReversals <- function(file,
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
  theme_set(theme_classic())
  if(group == TRUE) {
    metadata_path  = file.choose() |>
      dirname() |>
      fs::dir_ls(glob = "*metadata.csv")
    folder_path = dirname(metadata_path)
  } else { # this would be rare
    vid = vid
  }

  print(folder_path)

  ##### file checks ######
  #check if metadata are present
  if(basename(metadata_path) != "metadata.csv")
    stop("no metadata.csv in the selected folder")

  ## check if frame delay data are present ##
  frames_path <- fs::dir_ls(folder_path, glob = "*Frames.xlsx")
  if(length(frames_path) == 0)
    stop("no Frames.xlsx file present in folder")

  print(frames_path)

  frames_data <- readxl::read_xlsx(frames_path) %>%
    mutate(worm_index = as.factor(worm_index))

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
  alldata <- map_df(filenames, function(dataset) {
    features = H5Fopen(dataset)

    good_worms <- features$trajectories_data %>%
      filter(worm_label == 1) %>%
      pull(worm_index_joined) %>%
      unique()

    worm_data <- features$timeseries_data %>%
      filter(worm_index %in% good_worms) %>%
      mutate(realSpeed = speed) #add in adjustment if calibrated incorrectly

    tracks_plot <- ggplot(worm_data,
                          aes(x = coord_x_midbody,
                              y = coord_y_midbody,
                              colour = motion_mode)) +
      geom_point()

    # get video prefix for dataset joining
    video_pref <- paste0(substr(basename(dataset),nchar(basename(dataset))-33,nchar(basename(dataset))-20))

    #make an index column per worm:
    aligned_data <- worm_data |>
      #select relevant columns
      select(worm_index, timestamp,
             realSpeed,
             everything(),
             -well_name) |>
             # coord_x_midbody,
             # coord_y_midbody,
             # coord_x_head,
             # coord_y_head,
             # motion_mode,
             # curvature_mean_midbody,
             # curvature_mean_tail,
             # curvature_mean_head,
             # curvature_mean_neck,
             # curvature_neck,
             # curvature_midbody,
             # curvature_tail,
             # curvature_head,
             # d_curvature_head,
             # path_curvature_head,
             # path_curvature_midbody,
             # relative_to_body_radial_velocity_head_tip,
             # relative_to_body_angular_velocity_head_tip,
             # d_relative_to_body_radial_velocity_head_tip,
             # d_relative_to_body_angular_velocity_head_tip,
             # eigen_projection_1,
             # eigen_projection_2,
             # eigen_projection_3,
             # eigen_projection_4,
             # eigen_projection_5,
             # eigen_projection_6,
             # eigen_projection_7) |>
      # align by relative time
      filter(!is.na(speed)) |>
      group_by(worm_index) |>
      mutate(rowNum = row_number(),
             realTime = rowNum / framerate) |>
      filter(realTime < 100) |>
      mutate(worm_index = as.factor(worm_index)) |>
      mutate_if(is.integer, as.integer) |>
      mutate_if(is.double, as.double) |> # fix dbl[1d] list columns
      # align relative x and y of head
      group_by(worm_index) %>%
      filter(!any(realSpeed < -500)) |>
      # identify frame gaps
      mutate(frame_gap = c(1,diff(timestamp))) |>
      # mark each change of motiom_mode (will keep only those with 0 value)
      mutate(state_change = cumsum(replace_na(motion_mode != lag(motion_mode) & frame_gap == 1, 0))) |>
      #eliminate worms with wrong head-tail ID
      filter(first(motion_mode) == -1) |>
      # eliminate all worms with a substantial frame gap or change of motion state
      filter(state_change == 0) |>
      #filter(!any(frame_gap > 10)) |>
      mutate(relX_head = coord_x_head - first(coord_x_head),
             relY_head = coord_y_head - first(coord_y_head),
             # get midbody position relative to the head
             relX_body = coord_x_midbody - first(coord_x_head),
             relY_body = coord_y_midbody - first(coord_y_head),
             angle = atan2(last(relY_head), last(relX_head)),
             # rotated to start at 0,0 and backward movement is to the right
             x_rotated = relX_head*cos(-angle) - relY_head*sin(-angle),
             y_rotated = relX_head*sin(-angle) + relY_head*cos(-angle),
             x_body_rotated = relX_body*cos(-angle) - relY_body*sin(-angle),
             y_body_rotated = relX_body*sin(-angle) + relY_body*cos(-angle),
             # for merging with metadata
             video_pref = video_pref)


    reversal_plot <- aligned_data |>
      mutate(motion_mode = factor(motion_mode)) |>
      ggplot(aes(x = realTime, y = realSpeed)) +
      geom_line(aes(group = worm_index, colour = motion_mode)) +
      scale_color_viridis_d() +
      scale_x_continuous(limits = c(0,15)) +
      coord_cartesian(ylim = c(-500,500)) #+
    #annotate(geom = "text", x = 2.5, y = 200, label = paste0(genotype," : ",condition))


    ggsave(filename = file.path(results_path,paste0(video_pref,"_reversal_plot.png")), reversal_plot)
    ggsave(filename = file.path(results_path,paste0(video_pref,"_tracks_plot.png")), tracks_plot)


    #write_csv(aligned_data, file = paste0("/Users/mo555/Desktop/","aligned.csv"))
    h5closeAll()
    return(aligned_data)
  }, .id = "datafile")


  all_metadata <- full_join(metadata,frames_data)

  aligned_data <- full_join(alldata, all_metadata)

  ## align data by frame delay##

  aligned_data <- aligned_data %>%
    mutate(frame_delay = as.numeric(Fs) - as.numeric(F0),
      aligned_time = realTime + frame_delay / framerate)

  write_csv(aligned_data,
            file = file.path(results_path,
                             paste0(basename(results_path),"_aligned.csv")))
  #
  head_pos_plot <- aligned_data |>
    ggplot(aes(x = x_rotated, y = y_rotated)) +
    geom_path(aes(group = worm_index, colour = curvature_head), size = 0.5) +
    scale_color_viridis_c() +
    #scale_x_continuous(limits = c(0,15)) +
    coord_cartesian(ylim = c(-500,500)) +
    facet_grid(rows = vars({{compare1}}),
               cols = vars({{compare2}})) +
    theme_classic() +
    theme(panel.border = element_blank(),
          strip.background = element_blank())
  #annotate(geom = "text", x = 2.5, y = 200, label = paste0(genotype," : ",condition))

  speed_plot <- aligned_data |>
    filter(aligned_time < 1.26 & aligned_time > 0.1) %>%
    ggplot(aes( x = x_rotated, y = realSpeed)) +
    geom_path(aes(group = interaction(worm_index, video_pref),
                  colour = {{compare1}}), alpha =0.2) +
    scale_color_viridis_d(end = 0.9, begin = 0.1) +
    scale_fill_viridis_d(end = 0.9, begin = 0.1) +
    geom_smooth(aes(color = {{compare1}}), method = "lm") +
    geom_boxplot(aes(x = 700,
                     y = realSpeed,
                     fill = {{compare1}}),
                 outlier.shape = NA,
                 width = 100,
                 alpha = 0.8) +
    guides(color = "none") +
    facet_grid(rows = NULL,
               cols = vars({{compare2}})) +
    theme_classic() +
    theme(panel.border = element_blank(),
          strip.background = element_blank())

  #
  library(patchwork)
  summary_plot <- head_pos_plot / speed_plot
  ggsave(filename = file.path(results_path,"summary_plot.png"),
         summary_plot,
         width = 11,
         height = 8.5)
  return(summary_plot)

}
