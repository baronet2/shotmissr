devtools::load_all()

shots_data <- statsbomb_shots_processed |>
  prepare_shooting_skill_data()

mixture_model_components <- get_mixture_model_components()

pdfs <- get_shot_probability_densities(
  mixture_model_components,
  shots = shots_data |>
    dplyr::select(y_end_proj, z_end_proj) |>
    as.matrix()
)

global_weights <- fit_global_weights(pdfs, chains = 4, iter = 1000, seed = 42)

shooting_skill_data <- get_player_groups(
  shots_data,
  grouping_cols = c("player_id", "season"),
  group_size_threshold = 5
)

selected_components <- which(global_weights > 0.01)
usethis::use_data(selected_components)

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

out <- rstan::sampling(
  stanmodels$player_mm_weights,
  data = standata,
  chains = 4,
  iter = 1000,
  seed = 42
)

player_weight_posteriors <- rstan::extract(out)
usethis::use_data(player_weight_posteriors)
