Hunter_et_al_2018_shots <- read.csv("data-raw/Hunter_et_al_2018_shots.csv") %>%
  dplyr::select(foot, TargetHeightYards, HorizontalErrorYards, VerticalErrorYards) %>%
  dplyr::rename(
    target_height_yards = TargetHeightYards,
    horizontal_error_yards = HorizontalErrorYards,
    vertical_error_yards = VerticalErrorYards
  )

usethis::use_data(Hunter_et_al_2018_shots, overwrite = TRUE, internal = TRUE)
