load_data <- function(file_path) {
  read.csv(file_path, encoding = "UTF-8", header = TRUE) %>%
    dplyr::select(League, Season, player, position,
                  play_pattern, location, shot_technique, shot_type, shot_body_part, shot_statsbomb_xg,
                  shot_statsbomb_xg2, duration, shot_end_location, shot_outcome)
}

data_directory_path <- "data-raw/statsbomb_shots_raw"
files <- list.files(path = data_directory_path)
file_paths <- paste0(data_directory_path, "/", files)

statsbomb_shots <- lapply(file_paths, load_data) %>%
  # Merge into one data frame
  data.table::rbindlist() %>%
  clean_raw_statsbomb_data()

usethis::use_data(statsbomb_shots, overwrite = TRUE)

# TODO Follow instructions below
# For MLS_2018, FR2_18-19, GR2_18-19, NED_18-19: adjust end y and z coords, do not project end coords for saved shots
# For all other files: correct z coords only, do project end coords for saved shots
statsbomb_shots_processed <- statsbomb_shots %>%
  dplyr::mutate(
    do_adjust_y = Season <= 2018,
    do_project_saved = !do_adjust_y
  ) %>%
  adjust_shot_end_coords() %>%
  project_shot_end_coords() %>%
  dplyr::select(-do_adjust_y, -do_project_saved) %>%
  filter_shooting_skill_data()

usethis::use_data(statsbomb_shots_processed, overwrite = TRUE)
