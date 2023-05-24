devtools::load_all()

mixture_model_components <- get_mixture_model_components()

full_pdfs <- get_shot_probability_densities(
  mixture_model_components,
  shots = statsbomb_shots_processed |>
    dplyr::select(y_end_proj, z_end_proj) |>
    as.matrix()
)

for (xg_limit in c(0.1, 0.5, 1)) {
  for (distance_limit in c(0, 6, 15)) {
    indices_to_keep = which((statsbomb_shots_processed$SBPreXg <= xg_limit) &
      (statsbomb_shots_processed$distance >= distance_limit))

    print(glue::glue("xg_limit <= {xg_limit} & distance >= {distance_limit}: {length(indices_to_keep)} shots"))

    pdfs <- full_pdfs[indices_to_keep,]

    global_weights <- fit_global_weights(pdfs, iter = 500, seed = 42)
    saveRDS(global_weights, glue::glue("00_global_weights_{xg_limit}_{distance_limit}.rds"))

    selected_components <- which(global_weights > 0.01)

    half_season_shots_data <- statsbomb_shots_processed[indices_to_keep,] |>
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

    saveRDS(
      mixture_model_fit[["global_weights"]],
      glue::glue("01_global_weights_{xg_limit}_{distance_limit}.rds")
    )

    saveRDS(
      mixture_model_fit[["player_weights"]],
      glue::glue("01_player_weights_{xg_limit}_{distance_limit}.rds")
    )
  }
}

# component_values <- mixture_model_components[selected_components,] |>
#   get_component_values() |>
#   dplyr::pull(value)
#
# shot_metrics <- shooting_skill_data |>
#   load_rb_post_xg(player_weights, component_values) |>
#   load_gen_post_xg(pdfs, mixture_model_fit[["global_weights"]], component_values)
