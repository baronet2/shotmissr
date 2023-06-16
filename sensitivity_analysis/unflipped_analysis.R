devtools::load_all()
setwd("sensitivity_analysis")

mixture_model_components <- get_mixture_model_components()

full_pdfs <- get_shot_probability_densities(
  mixture_model_components,
  shots = statsbomb_shots_processed |>
    dplyr::select(y_end_proj, z_end_proj) |>
    as.matrix()
)

results <- expand.grid(
  xg_limit = c(0.1, 0.5, 1),
  distance_limit = c(0, 6, 15)
) |>
  dplyr::mutate(
    indices_to_keep = purrr::map2(
      xg_limit, distance_limit,
      ~ which((statsbomb_shots_processed$SBPreXg <= .x) & (statsbomb_shots_processed$distance >= .y))
    ),
    global_weights_00 = purrr::map2(
      xg_limit, distance_limit,
      ~ readRDS(glue::glue("00_global_weights_{.x}_{.y}.rds"))
    ),
    selected_components = purrr::map(global_weights_00, ~ which(.x > 0.01)),
    half_season_shots_data = purrr::map(
      indices_to_keep,
      ~ statsbomb_shots_processed[.x,] |>
        dplyr::group_by(player, Season) |>
        dplyr::mutate(first_half_season = dplyr::row_number() < dplyr::n() / 2) |>
        dplyr::ungroup()
    ) ,
    shooting_skill_data = purrr::map(
      half_season_shots_data,
      ~ get_player_groups(
        .x,
        grouping_cols = c("player", "Season", "first_half_season"),
        group_size_threshold = 5
      )
    ),
    pdfs = purrr::map2(
      shooting_skill_data, selected_components,
      ~ get_shot_probability_densities(
        mixture_model_components[.y,],
        .x |> dplyr::select(y_end_proj, z_end_proj) |> as.matrix()
      )
    ),
    global_weights = purrr::map2(
      xg_limit, distance_limit,
      ~ readRDS(glue::glue("01_global_weights_{.x}_{.y}.rds"))
    ),
    player_weights = purrr::map2(
      xg_limit, distance_limit,
      ~ readRDS(glue::glue("01_player_weights_{.x}_{.y}.rds"))
    ),
    component_values = purrr::map(
      selected_components,
      ~ mixture_model_components[.x,] |>
        get_component_values() |>
        dplyr::pull(value)
    ),
    shot_metrics = purrr::pmap(
      list(shooting_skill_data, player_weights, component_values, pdfs, global_weights),
      ~ ..1 |>
        load_rb_post_xg(..2, ..3) |>
        load_gen_post_xg(..4, ..5, ..3)
    ),
    half_season_stats = purrr::map(
      shot_metrics,
      ~ .x |>
        dplyr::mutate(SBPostXg = ifelse(is.na(SBPostXg), 0, SBPostXg)) |>
        dplyr::mutate(goal_pct = (outcome == "Goal")) |>
        dplyr::mutate(
          gax = goal_pct - SBPreXg,
          ega = SBPostXg - SBPreXg
        ) |>
        dplyr::group_by(player, Season, first_half_season) |>
        dplyr::summarise(
          dplyr::across(dplyr::matches("goal_pct|gax|ega|_xg"), mean, na.rm = TRUE),
          n = dplyr::n(),
          .groups = "keep"
        )
    ),
    stability_data = purrr::map(
      half_season_stats,
      ~ .x |>
        dplyr::inner_join(.x, by = c("player", "Season"), suffix = c("_a", "_b")) |>
        dplyr::filter(first_half_season_a, !first_half_season_b) |>
        dplyr::ungroup()
    )
  ) |>
  dplyr::select(xg_limit, distance_limit, stability_data)

correlations <- results |>
  dplyr::mutate(metric = list(c("gax", "ega", "rb_post_xg", "gen_post_xg"))) |>
  tidyr::unnest(metric) |>
  dplyr::mutate(threshold = list(6:60)) |>
  tidyr::unnest(threshold) |>
  dplyr::mutate(
    filtered_data = purrr::map2(stability_data, threshold, ~ dplyr::filter(.x, n_a + n_b >= .y)),
    correlation = purrr::map2_dbl(filtered_data, metric,
      ~ cor(.x[[paste0(.y, "_a")]], .x[[paste0(.y, "_b")]])
    )
  )

get_plot <- function(stability_data) {
  get_stability_above_threshold <- function(metric, n) {
    filtered_data <- stability_data |>
      dplyr::filter(n_a + n_b >= n)
    cor(filtered_data[[paste0(metric, "_a")]], filtered_data[[paste0(metric, "_b")]])
  }

  expand.grid(threshold = 6:60, metric = c("gax", "ega", "rb_post_xg", "gen_post_xg")) |>
    dplyr::mutate(
      stability = purrr::map2_dbl(threshold, metric, ~ get_stability_above_threshold(.y, .x))
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = threshold, y= stability, colour = metric)) +
    ggplot2::geom_line() +
    ggplot2::theme_bw()
}

correlations |>
  dplyr::mutate(
    metric = dplyr::case_when(
      metric == "gax" ~ "GAX",
      metric == "ega" ~ "EGA",
      metric == "rb_post_xg" ~ "RBPostXg",
      metric == "gen_post_xg" ~ "GenPostXg"
    )
  ) |>
  dplyr::rename(
    `Minimum Shot Distance` = distance_limit,
    `Maximum Pre-Shot xG` = xg_limit,
    Metric = metric
  ) |>
  ggplot2::ggplot(ggplot2::aes(x = threshold, y = correlation, colour = Metric)) +
  ggplot2::geom_line() +
  ggplot2::theme_light() +
  ggplot2::facet_grid(`Maximum Pre-Shot xG` ~ `Minimum Shot Distance`, labeller = "label_both") +
  ggplot2::labs(
    x = "Minimum Sample Size",
    y = "Correlation"
  )

ggplot2::ggsave(
  "unflipped_plots.png",
  width = 8,
  height = 6
)
