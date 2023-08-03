devtools::load_all()

mixture_model_components <- get_mixture_model_components()

pdfs <- get_shot_probability_densities(
  mixture_model_components,
  shots = statsbomb_shots_processed |>
    dplyr::select(y_end_proj, z_end_proj) |>
    as.matrix()
)

global_weights <- fit_global_weights(pdfs, iter = 500, seed = 42)
selected_components <- which(global_weights > 0.01)
usethis::use_data(selected_components, overwrite = TRUE)

# Differences: prepare_data instead of filter out null, group by season but not league, pruning threshold...
half_season_shots_data <- statsbomb_shots_processed |>
  dplyr::group_by(player, Season) |>
  dplyr::mutate(first_half_season = dplyr::row_number() < dplyr::n() / 2) |>
  dplyr::ungroup()

shooting_skill_data <- get_player_groups(
  half_season_shots_data,
  grouping_cols = c("player", "Season", "first_half_season"),
  group_size_threshold = 5
)

pdfs <- get_shot_probability_densities(
  mixture_model_components[selected_components,],
  shooting_skill_data |> dplyr::select(y_end_proj, z_end_proj) |> as.matrix()
)

mixture_model_fit <- fit_player_weights(
  pdfs,
  shooting_skill_data$group_id,
  alpha = 30,
  iter = 500,
  seed = 42
)

global_weights <- mixture_model_fit[["global_weights"]]
usethis::use_data(global_weights, overwrite = TRUE)

player_weights <- mixture_model_fit[["player_weights"]]
usethis::use_data(player_weights, overwrite = TRUE)


component_values <- mixture_model_components[selected_components,] |>
  get_component_values() |>
  dplyr::pull(value)

shot_metrics <- shooting_skill_data |>
  load_rb_post_xg(player_weights, component_values) |>
  load_gen_post_xg(pdfs, mixture_model_fit[["global_weights"]], component_values)

usethis::use_data(shot_metrics, overwrite = TRUE)
