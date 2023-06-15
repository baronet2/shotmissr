Miss It Like Messi
================

``` r
devtools::load_all()
#> i Loading shotmissr
```

## Table 1

``` r
statsbomb_shots_processed |>
  dplyr::group_by(League, Season) |>
  dplyr::summarise(num_shots = dplyr::n(), .groups = "keep") |>
  knitr::kable()
```

| League | Season | num_shots |
|:-------|-------:|----------:|
| ARG    |   2019 |      3409 |
| FR2    |   2018 |      4438 |
| FR2    |   2019 |      3290 |
| FR2    |   2020 |      4300 |
| GR2    |   2018 |      4086 |
| GR2    |   2019 |      4099 |
| GR2    |   2020 |      3881 |
| MLS    |   2018 |      5857 |
| MLS    |   2019 |      6220 |
| MLS    |   2020 |      4294 |
| NED    |   2018 |      4571 |
| NED    |   2019 |      3384 |
| NED    |   2020 |      4198 |
| USL    |   2019 |      2677 |
| USL    |   2020 |      3823 |

## Figure 1

``` r
statsbomb_shots_processed |>
  dplyr::filter(League == "MLS", Season == 2018) |>
  dplyr::filter(!is.na(z_end)) |>
  dplyr::rename(Outcome = outcome) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(mapping = ggplot2::aes(
    x = y_end,
    y = z_end,
    color = Outcome)) +
  plot_goalposts(color = "red", cex = 2, alpha = 0.2) +
  ggplot2::theme_light() +
  ggplot2::xlim(30, 50) +
  ggplot2::ylim(0, 8) +
  ggplot2::labs(
    x = "Shot End y-coordinate",
    y = "Shot End z-coordinate"
  ) +
  ggplot2::theme(legend.position = c(0.1, 0.85))
```

![](miss_it_like_messi_files/figure-gfm/figure_1-1.png)<!-- -->

## Figure 2

``` r
statsbomb_shots_processed |>
  dplyr::filter(League == "USL", Season == 2020) |>
  dplyr::filter(!is.na(z_end)) |>
  dplyr::filter(grepl("Saved", outcome)) |>
  dplyr::rename(Outcome = outcome) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(mapping = ggplot2::aes(
    x = y_end,
    y = z_end,
    color = Outcome)) +
  plot_goalposts(color = "red", cex = 2, alpha = 0.2) +
  ggplot2::theme_light() +
  ggplot2::xlim(30, 50) +
  ggplot2::ylim(0, 3) +
  ggplot2::labs(
    x = "Shot End y-coordinate",
    y = "Shot End z-coordinate"
  ) +
  ggplot2::theme(legend.position = c(0.1, 0.85))
#> Warning: Removed 13 rows containing missing values (geom_point).
```

![](miss_it_like_messi_files/figure-gfm/figure_2-1.png)<!-- -->

``` r

statsbomb_shots_processed |>
  dplyr::filter(League == "USL", Season == 2020) |>
  dplyr::filter(!is.na(z_end)) |>
  dplyr::filter(grepl("Saved", outcome)) |>
  dplyr::rename(Outcome = outcome) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(mapping = ggplot2::aes(
    x = y_end_proj,
    y = z_end_proj,
    color = Outcome)) +
  plot_goalposts(color = "red", cex = 2, alpha = 0.2) +
  ggplot2::theme_light() +
  ggplot2::xlim(30, 50) +
  ggplot2::ylim(0, 3) +
  ggplot2::labs(
    x = "Shot End y-coordinate",
    y = "Shot End z-coordinate"
  ) +
  ggplot2::theme(legend.position = c(0.1, 0.85))
#> Warning: Removed 2 rows containing missing values (geom_point).
```

![](miss_it_like_messi_files/figure-gfm/figure_2-2.png)<!-- -->

## Figure 5

``` r
yy <- seq(y_left_post(), y_right_post(), by = 0.1)
zz <- seq(0, z_crossbar(), by = 0.03)
shots <- expand.grid(y = yy, z = zz)
shots |>
  dplyr::mutate(post_shot_xg = predict_post_xg(y, z)) |>
  ggplot2::ggplot() +
  ggplot2::geom_contour_filled(
    mapping = ggplot2::aes(x = y, y = z, z = post_shot_xg),
    bins = 100,
    show.legend = FALSE
  ) +
  plot_goalposts(color = "red", cex = 2) +
  ggplot2::theme_light() +
  ggplot2::xlim(35, 45) +
  ggplot2::ylim(0, 3) +
  ggplot2::labs(
    x = "Shot End y-coordinate",
    y = "Shot End z-coordinate"
  )
```

![](miss_it_like_messi_files/figure-gfm/figure_5-1.png)<!-- -->

## Figure 7

``` r
statsbomb_shots |>
  dplyr::filter(League == "MLS", Season == 2018) |>
  dplyr::filter(!is.na(z_end)) |>
  dplyr::rename(Outcome = outcome) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(mapping = ggplot2::aes(
    x = y_end,
    y = z_end,
    color = Outcome)) +
  plot_goalposts(color = "red", cex = 2, alpha = 0.2) +
  ggplot2::theme_light() +
  ggplot2::xlim(30, 50) +
  ggplot2::ylim(0, 8) +
  ggplot2::labs(
    x = "Shot End y-coordinate",
    y = "Shot End z-coordinate"
  ) +
  ggplot2::theme(legend.position = c(0.1, 0.85))
```

![](miss_it_like_messi_files/figure-gfm/figure_7-1.png)<!-- -->

## Figure 8

``` r
z_target <- sort(unique(Hunter_et_al_2018_shots$target_height_yards))
cat("Execution error covariance matrices:")
#> Execution error covariance matrices:
get_execution_error_covariance(z_target[1])
#>           [,1]      [,2]
#> [1,] 0.7036574 0.1566326
#> [2,] 0.1566326 0.2969795
get_execution_error_covariance(z_target[2])
#>           [,1]      [,2]
#> [1,] 0.7815552 0.4417238
#> [2,] 0.4417238 0.7419683

yy = seq(34, 46, length.out = 100)
zz = seq(0, 4, length.out = 100)
yz = expand.grid(y = yy, z = zz)

gaussians <- yz |>
  dplyr::mutate(
    prob_low = tmvtnorm::dtmvnorm(
      x = as.matrix(yz),
      mean = c(y_center_line(), z_target[1]), 
      sigma = get_execution_error_covariance(z_target[1]), 
      lower = c(-Inf, 0)
    ),
    prob_high = tmvtnorm::dtmvnorm(
      x = as.matrix(yz),
      mean = c(y_center_line(), z_target[2]), 
      sigma = get_execution_error_covariance(z_target[2]), 
      lower = c(-Inf, 0)
    )
  )

Hunter_et_al_2018_shots |>
  # Get low-target shots
  dplyr:::filter(target_height_yards == z_target[1]) |>
  dplyr::mutate(
    y_end = y_center_line() + horizontal_error_yards,
    z_end = target_height_yards + vertical_error_yards
  ) |>
  ggplot2::ggplot() +
  # Add contour plot of Gaussian
  ggplot2::geom_contour_filled(
    data = gaussians,
    mapping = ggplot2::aes(x = y, y = z, z = prob_low),
    breaks = seq(0.01, 0.7, by = 0.07),
    show.legend = FALSE
  ) +
  # Make Gaussian contour colour blue
  ggplot2::scale_fill_brewer() +
  # Add observed shots
  ggplot2::geom_point(
    mapping = ggplot2::aes(
      x = y_end,
      y = z_end), 
    colour = "orange", size = 0.5
  ) +
  # Add target location
  ggplot2::geom_point(x = y_center_line(), y = z_target[1], colour = "red") +
  plot_goalposts(color = "red", cex = 2, alpha = 0.2) +
  ggplot2::theme_light() +
  ggplot2::xlim(35, 45) +
  ggplot2::ylim(0, 4.5) +
  ggplot2::labs(
    x = "Shot End y-coordinate",
    y = "Shot End z-coordinate"
  )
#> Warning: Removed 1800 rows containing non-finite values (stat_contour_filled).
#> Warning: Removed 1 rows containing missing values (geom_point).
```

![](miss_it_like_messi_files/figure-gfm/figure_8-1.png)<!-- -->

``` r

Hunter_et_al_2018_shots |>
  # Get high-target shots
  dplyr:::filter(target_height_yards == z_target[2]) |>
  dplyr::mutate(
    y_end = y_center_line() + horizontal_error_yards,
    z_end = target_height_yards + vertical_error_yards
  ) |>
  ggplot2::ggplot() +
  # Add contour plot of Gaussian
  ggplot2::geom_contour_filled(
    data = gaussians,
    mapping = ggplot2::aes(x = y, y = z, z = prob_high),
    breaks = seq(0.01, 0.3, by = 0.03),
    show.legend = FALSE
  ) +
  # Make Gaussian contour colour blue
  ggplot2::scale_fill_brewer() +
  # Add observed shots
  ggplot2::geom_point(
    mapping = ggplot2::aes(
      x = y_end,
      y = z_end), 
    colour = "orange", size = 0.5
  ) +
  # Add target location
  ggplot2::geom_point(x = y_center_line(), y = z_target[2], colour = "red") +
  plot_goalposts(color = "red", cex = 2, alpha = 0.2) +
  ggplot2::theme_light() +
  ggplot2::xlim(35, 45) +
  ggplot2::ylim(0, 4.5) +
  ggplot2::labs(
    x = "Shot End y-coordinate",
    y = "Shot End z-coordinate"
  )
#> Warning: Removed 1800 rows containing non-finite values (stat_contour_filled).
#> Warning: Removed 5 rows containing missing values (geom_point).
```

![](miss_it_like_messi_files/figure-gfm/figure_8-2.png)<!-- -->

## Figure 3

``` r
mixture_model_components <- get_mixture_model_components()

mixture_model_components[selected_components,] |>
  dplyr::mutate(
    y = purrr::map_dbl(mean, ~.[[1]]),
    z = purrr::map_dbl(mean, ~.[[2]]),
    weight = global_weights,
    lambda = as.factor(lambda)
  ) |>
  dplyr::filter(weight > 0.012) |>
  dplyr::rename(Weight = weight) |>
  ggplot2::ggplot(ggplot2::aes(x = y, y = z, alpha = Weight, size = lambda)) +
  ggplot2::geom_point(colour = "blue") +
  ggplot2::scale_size_manual(values = c(3, 7)) +
  plot_goalposts(color = "red", cex = 2, alpha = 0.2) +
  ggplot2::theme_light() +
  ggplot2::xlim(35, 45) +
  ggplot2::ylim(0, 3) +
  ggplot2::labs(
    x = "Shot End y-coordinate",
    y = "Shot End z-coordinate"
  )
```

![](miss_it_like_messi_files/figure-gfm/figure_3-1.png)<!-- -->

## Calculate player metrics

``` r
half_season_stats <- shot_metrics |>
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
```

## Table 3

``` r
stability_data <- half_season_stats |>
  dplyr::inner_join(half_season_stats, by = c("player", "Season"), suffix = c("_a", "_b")) |>
  dplyr::filter(first_half_season_a, !first_half_season_b) |>
  dplyr::ungroup()

stability_data |>
  # Get metrics only
  dplyr::select(
    dplyr::starts_with("goal_pct"),
    dplyr::starts_with("gax"),
    dplyr::starts_with("ega"),
    dplyr::matches("_xg")
  ) |>
  # Get correlation matrix
  cor() |>
  # Subset correlation matrix so season A is rows and season B is columns
  data.frame() |>
  dplyr::select(dplyr::ends_with("_a")) |>
  t() |>
  data.frame() |>
  dplyr::select(dplyr::ends_with("_b")) |>
  as.matrix() |>
  knitr::kable()
```

|               | goal_pct_b |     gax_b |      ega_b | rb_post_xg_b | gen_post_xg_b |
|:--------------|-----------:|----------:|-----------:|-------------:|--------------:|
| goal_pct_a    |  0.0504398 | 0.0479698 |  0.0099914 |    0.0116243 |     0.0314498 |
| gax_a         |  0.0434784 | 0.0444193 | -0.0000781 |    0.0004993 |     0.0207347 |
| ega_a         |  0.0371670 | 0.0322199 |  0.0072857 |    0.0302872 |     0.0171759 |
| rb_post_xg_a  |  0.0235098 | 0.0189044 |  0.0313871 |    0.1297171 |     0.1205273 |
| gen_post_xg_a |  0.0276133 | 0.0210376 |  0.0376509 |    0.1380937 |     0.1515649 |

## Table 4

``` r
stability_data |>
  dplyr::filter(n_a + n_b >= 30) |>
  # Get metrics only
  dplyr::select(
    dplyr::starts_with("goal_pct"),
    dplyr::starts_with("gax"),
    dplyr::starts_with("ega"),
    dplyr::matches("_xg")
  ) |>
  # Get correlation matrix
  cor() |>
  # Subset correlation matrix so season A is rows and season B is columns
  data.frame() |>
  dplyr::select(dplyr::ends_with("_a")) |>
  t() |>
  data.frame() |>
  dplyr::select(dplyr::ends_with("_b")) |>
  as.matrix() |>
  knitr::kable()
```

|               | goal_pct_b |      gax_b |     ega_b | rb_post_xg_b | gen_post_xg_b |
|:--------------|-----------:|-----------:|----------:|-------------:|--------------:|
| goal_pct_a    |  0.0909079 |  0.0731302 | 0.1085867 |   -0.0132172 |     0.0548137 |
| gax_a         |  0.0769423 |  0.0614395 | 0.0898154 |   -0.0014298 |     0.0534517 |
| ega_a         | -0.0320833 | -0.0411309 | 0.0630560 |    0.1397886 |     0.1415280 |
| rb_post_xg_a  |  0.0064516 |  0.0050224 | 0.1288577 |    0.1698563 |     0.2197126 |
| gen_post_xg_a | -0.0253883 | -0.0219570 | 0.0736002 |    0.1779928 |     0.2351998 |

``` r
stability_data |>
  dplyr::filter(n_a + n_b >= 50)
#> # A tibble: 9 x 16
#>   player   Season first~1 rb_po~2 gen_p~3 goal_~4   gax_a    ega_a   n_a first~5
#>   <fct>     <int> <lgl>     <dbl>   <dbl>   <dbl>   <dbl>    <dbl> <int> <lgl>  
#> 1 Aleksan~   2019 TRUE     0.0589  0.0591  0.0741  0.0423  0.0216     27 FALSE  
#> 2 Carlos ~   2019 TRUE     0.0610  0.0607  0.0645  0.0259  0.0196     31 FALSE  
#> 3 Gonzalo~   2019 TRUE     0.0590  0.0572  0      -0.0357 -0.0162     24 FALSE  
#> 4 Hakim Z~   2018 TRUE     0.0616  0.0618  0.143   0.112   0.0295     42 FALSE  
#> 5 Hakim Z~   2019 TRUE     0.0611  0.0617  0.08    0.0426  0.0376     25 FALSE  
#> 6 Miguel ~   2018 TRUE     0.0593  0.0586  0.0667  0.0226  0.00213    30 FALSE  
#> 7 Oussama~   2020 TRUE     0.0616  0.0605  0.0526  0.0204 -0.00320    38 FALSE  
#> 8 Sebasti~   2018 TRUE     0.0580  0.0566  0.0278 -0.0138  0.0182     36 FALSE  
#> 9 Steven ~   2020 TRUE     0.0616  0.0618  0.103   0.0682  0.00110    29 FALSE  
#> # ... with 6 more variables: rb_post_xg_b <dbl>, gen_post_xg_b <dbl>,
#> #   goal_pct_b <dbl>, gax_b <dbl>, ega_b <dbl>, n_b <int>, and abbreviated
#> #   variable names 1: first_half_season_a, 2: rb_post_xg_a, 3: gen_post_xg_a,
#> #   4: goal_pct_a, 5: first_half_season_b
```

## Figure 6

``` r
get_stability_above_threshold <- function(metric, n) {
  filtered_data <- stability_data |>
    dplyr::filter(n_a + n_b >= n)
  
  # TODO bootstrap rows here to get estimate of lower and upper bound
  
  cor(filtered_data[[paste0(metric, "_a")]], filtered_data[[paste0(metric, "_b")]])
}

expand.grid(threshold = 6:60, metric = c("gax", "ega", "rb_post_xg", "gen_post_xg")) |>
  dplyr::mutate(
    stability = purrr::map2_dbl(threshold, metric, ~ get_stability_above_threshold(.y, .x))
  ) |>
  ggplot2::ggplot(ggplot2::aes(x = threshold, y= stability, colour = metric)) +
  ggplot2::geom_line() +
  ggplot2::theme_light()
```

![](miss_it_like_messi_files/figure-gfm/figure_6-1.png)<!-- -->

``` r
# TODO Use geom_ribbon here to add custom error bars returned by get_stability_above_threshold
```
