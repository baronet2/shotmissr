#' Adjust shot end coordinates
#'
#' Adjust the shot end coordinates to fix bias in data collection process for shots
#' near the posts.
#'
#' @param data A data frame with shot details and the original shot end coordinates
#'
#' @return The same data frame with corrected shot end coordinate columns
#'
#' @export
adjust_shot_end_coords <- function(data)
{

  .adjust_x_end <- function(data) {
    data %>%
      dplyr::mutate(
        x_end = ifelse(outcome %in% c('Goal', 'Off T', 'Post'), x_goal_line(), x_end)
      )
  }

  .adjust_y_end <- function(data) {
    data %>%
      dplyr::mutate(
        condition = dplyr::case_when(
          z_end > 3.2 ~ 1, # High ball - don't adjust
          y_end < 35.6 ~ 2, # Left of goal
          y_end <= 35.8  ~ 3, # Left post
          y_end <= 44 ~ 4, # dplyr::between posts
          y_end < 44.8 ~ 5, # Right post
          TRUE ~ 6 # Right of goal
        ),
      ) %>%
      dplyr::group_by(do_adjust_y, condition) %>%
      dplyr::mutate(
        y_end = dplyr::case_when(
          !do_adjust_y ~ y_end,
          x_end != x_goal_line() ~ y_end,
          condition == 1 ~ y_end,
          condition == 2 ~ scales::rescale(y_end, c(min(y_end, na.rm = TRUE), 35.85)),
          condition == 3 ~ scales::rescale(y_end, c(35.85, 36)),
          condition == 4 ~ scales::rescale(y_end, c(36, 43.9)),
          condition == 5 ~ scales::rescale(y_end, c(44, 44.15)),
          condition == 6 ~ scales::rescale(y_end, c(44.15, max(y_end, na.rm = TRUE)))
        )
      ) |>
      dplyr::ungroup() |>
      dplyr::select(-condition)
  }

  .adjust_z_end <- function(data) {
    data  |>
      dplyr::mutate(
        condition = dplyr::case_when(
          dplyr::between(z_end, 1.3, 2.4) & dplyr::between(y_end, 35.6, 44.6) ~ 1,
          dplyr::between(z_end, 2.7, 3) ~ 2,
          dplyr::between(z_end, 3, 3.2) ~ 3,
          z_end > 3.2 ~ 4,
          TRUE ~ 5
        )
      ) |>
      dplyr::group_by(condition) |>
      dplyr::mutate(
        z_end = dplyr::case_when(
          condition == 1 ~ scales::rescale(z_end, c(1.2, 2.6)),
          condition == 2 ~ scales::rescale(z_end, c(2.7, 2.8)),
          condition == 3 ~ scales::rescale(z_end, c(2.8, 2.9)),
          condition == 4 ~ scales::rescale(z_end, c(2.9, 7.8)),
          condition == 5 ~ z_end
        )
      ) |>
      dplyr::ungroup() |>
      dplyr::select(-condition)
  }

  .adjust_z_start <- function(data) {
    data  |>
      # Fill NA z_start with mean z_Start for that body-part and technique
      dplyr::group_by(body_part, technique) |>
      dplyr::mutate(
        z_start = dplyr::case_when(
          is.na(z_start) ~ mean(z_start, na.rm = TRUE),
          TRUE ~ z_start
        )
      ) |>
      dplyr::ungroup() |>
      # If still NA, use mean z_Start for that body-part
      dplyr::group_by(body_part) |>
        dplyr::mutate(
          z_start = dplyr::case_when(
            is.na(z_start) ~ mean(z_start, na.rm = TRUE),
            TRUE ~ z_start
          )
        ) |>
        dplyr::ungroup()
  }

  data %>%
    .adjust_x_end() |>
    .adjust_y_end() |>
    .adjust_z_end() |>
    .adjust_z_start()
}


#' Project shot end coordinates
#'
#' @description Project the shot end coordinates for shots with \code{x_end} != 120,
#' primarily saved shots. Uses projectile motion equations to impute the shot
#' trajectory and infer the shot end location.
#'
#' @param data A data frame with shot details and the original shot end coordinates
#'
#' @return The same data frame with new columns \code{y_end_proj} and \code{z_end_proj}
#'
#' @export
project_shot_end_coords <- function(data)
{
  data %>%
    dplyr::mutate(
      y_end_proj = y_start + (x_goal_line() - x_start) * (y_end - y_start) / (x_end - x_start),
      duration = pmin(duration, 4), # 4 from looking at histogram of original durations
      t_end_proj = duration * (x_goal_line() - x_start) / (x_end - x_start),
      # TODO Finish implementing bouncing
      # See https://github.com/baronet2/UofT-TFC-Off-Target-Shots/blob/main/utilities/features/shot_trajectory.py
      # z_velocity_start = (z_end - z_start + (gravity() / 2) * (duration ^ 2)) / duration,
      z_end_proj = z_end
    ) |>
    dplyr::mutate(
      y_end_proj = ifelse(x_start < x_end, y_end_proj, NA_real_),
      z_end_proj = ifelse(x_start < x_end, z_end_proj, NA_real_)
    ) |>
    dplyr::mutate(
      y_end_proj = ifelse(do_project_saved, y_end_proj, y_end),
      z_end_proj = ifelse(do_project_saved, z_end_proj, z_end)
    ) |>
    dplyr::select(-c(duration, t_end_proj))
}


#' Filter shooting skill data
#'
#' @description Remove rows from the shots data that are not used to measure shooting skill.
#' Specifically, keep only shots from at least 15 yards out that have a StatsBomb
#' pre-shot expected goals value of less than 0.1 and valid shot end coordinates.
#'
#' @param data A data frame with shot details
#'
#' @return The same data frame with some rows removed
#'
#' @export
filter_shooting_skill_data <- function(data)
{
  euclidean_dist <- function(x1, y1, x2, y2) {
    sqrt((x2 - x1)^2 + (y2 - y1)^2)
  }

  data %>%
    dplyr::mutate(
      distance = euclidean_dist(
        x_start, y_start, x_goal_line(),
        dplyr::case_when(
          y_start < y_left_post() ~ y_left_post(),
          y_start > y_right_post() ~ y_right_post(),
          TRUE ~ y_start
        )
      )
    ) %>%
    dplyr::filter(
      !is.na(y_end_proj),
      !is.na(z_end_proj),
      # SBPreXg < 0.1,
      distance >= 6,
      SBPreXg < 0.5,
      grepl("Foot", body_part),
      grepl("Open Play", type)
    )
}


#' Flip left-footed shot end coordinates
#'
#' Reflect the shot end coordinates for left-footed shots about the centre of the net.
#'
#' @param data A data frame with shot details and the original shot end coordinates
#'
#' @return The same data frame with the \code{y_end_proj} column changed.
#'
#' @export
flip_left_foot_shot_end_coords <- function(data)
{
  data %>%
    dplyr::mutate(
      # y_end_proj = ifelse(
      #   body_part == "Left Foot",
      #   2 * y_center_line() - y_end_proj,
      #   y_end_proj
      # )
      y_end_proj = y_end_proj
    )
}


#' Prepare shooting skill data
#'
#' @description Prepare data used to measure shooting skill. Some rows are removed
#' and left-footed shots are reflected.
#'
#' @param data A data frame with shot details
#'
#' @return The data frame modified for use in the shooting skill modeling pipeline
#'
#' @export
prepare_shooting_skill_data <- function(data)
{
  data %>%
    filter_shooting_skill_data() %>%
    flip_left_foot_shot_end_coords()
}


#' Clean raw StatsBomb data
#'
#' @description Reformat columns from raw StatsBomb shot data
#'
#' @param data A data frame with shot details, and the original columns from StatsBomb data
#'
#' @return The data frame modified for use in the shooting skill modeling pipeline
#'
#' @export
clean_raw_statsbomb_data <- function(data) {
  data %>%
    dplyr::rename(
      SBPreXg = shot_statsbomb_xg,
      SBPostXg = shot_statsbomb_xg2
    ) %>%
    dplyr::rename_all(
      ~stringr::str_replace(., "^shot_", "")
    ) %>%
    dplyr::mutate(
      location = gsub('^.|.$', '', location),
      end_location = gsub('^.|.$', '', end_location)
    ) %>%
    tidyr::separate(
      location,
      c("x_start", "y_start", "z_start"),
      sep = ", ",
      convert = TRUE,
      fill = "right"
    ) %>%
    tidyr::separate(
      end_location,
      c("x_end", "y_end", "z_end"),
      sep = ", ",
      convert = TRUE,
      fill = "right"
    ) %>%
    dplyr::mutate(player = stringr::str_trim(player)) |>
    dplyr::mutate_if(is.character, as.factor)
}
