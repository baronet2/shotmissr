execution_error_fits <- Hunter_et_al_2018_shots %>%
  flip_left_foot_Hunter_shots() %>%
  dplyr::group_nest(target_height_yards) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    var_xx = var(data$horizontal_error_yards),
    var_yy = var(data$vertical_error_yards),
    var_xy = cov(data$horizontal_error_yards, data$vertical_error_yards)
  )

execution_error_linear_fits <- data.frame(thing = c("var_xx", "var_yy", "var_xy")) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    formula = paste0(thing, "~ target_height_yards"),
    model = list(lm(
      formula = formula,
      data = execution_error_fits
    ))
  )

usethis::use_data(execution_error_linear_fits, overwrite = TRUE, internal = TRUE)
