devtools::load_all()

shots_data <- statsbomb_shots_processed |>
  prepare_shooting_skill_data() |>
  dplyr::filter(League == "MLS", Season == 2018)

mixture_model_components <- get_mixture_model_components()

pdfs <- get_shot_probability_densities(
  mixture_model_components,
  shots = shots_data |>
    dplyr::select(y_end_proj, z_end_proj) |>
    as.matrix()
)

global_weights <- fit_global_weights(
  pdfs,
  iter = 500,
  seed = 42
)

shooting_skill_data <- get_player_groups(
  shots_data,
  grouping_cols = c("player", "Season"),
  group_size_threshold = 5
)

selected_components <- which(global_weights > 0.01)
usethis::use_data(selected_components, overwrite = TRUE)

pdfs <- get_shot_probability_densities(
  mixture_model_components[selected_components,],
  shooting_skill_data |> dplyr::select(y_end_proj, z_end_proj) |> as.matrix()
)

standata <- list(
  num_players = max(shooting_skill_data$group_id),
  num_shots = nrow(pdfs),
  num_components = ncol(pdfs),
  shot_players = shooting_skill_data$group_id,
  trunc_pdfs = pdfs,
  alpha = 30
)

out <- rstan::vb(
  stanmodels$player_mm_weights,
  data = standata,
  iter = 500,
  seed = 42
)

player_weight_posteriors <- rstan::extract(out)
usethis::use_data(player_weight_posteriors, overwrite = TRUE)
