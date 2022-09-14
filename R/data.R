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
  # TODO Implement. See https://github.com/baronet2/UofT-TFC-Off-Target-Shots/blob/main/utilities/data_preparer.py
  data %>%
    dplyr::mutate(
      x_end = ifelse(outcome %in% c('Goal', 'Off T', 'Post'), x_end, x_goal_line())
    )
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
  # TODO Implement. See https://github.com/baronet2/UofT-TFC-Off-Target-Shots/blob/main/utilities/features/shot_trajectory.py
  data %>%
    dplyr::mutate(
      y_end_proj = y_end,
      z_end_proj = z_end
    )
}


#' Filter shooting skill data
#'
#' @description Remove rows from the shots data that are not used to measure shooting skill.
#' Specifically, keep only shots that are at least 15 yards from the goal and have a StatsBomb
#' pre-shot expected goals value of less than 0.1.
#'
#' @param data A data frame with shot details
#'
#' @return The same data frame with some rows removed
filter_shooting_skill_data <- function(data)
{
  data %>%
    dplyr::filter(
      distance >= 15,
      SBPreXg < 0.1
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
      y_end_proj = ifelse(y_end_proj > center_line_y(),
                          y_end_proj,
                          2 * center_line_y() - y_end_proj)
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
    flip_left_foot_shots()
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
    dplyr::mutate_if(is.character, as.factor)
}
