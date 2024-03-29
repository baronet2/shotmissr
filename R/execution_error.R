#' Get covariance matrix of execution error
#'
#' Get the covariance matrix of exeucution error in the (y, z) end coordinates
#' for soccer shots aimed at a given height.
#'
#' @param height A numeric value, the intended shot height in yards
#'
#' @return A 2x2 covariance matrix
#'
#' @examples
#' get_execution_error_covariance(1.25)
#'
#' @export
get_execution_error_covariance <- function(height)
{
  predictions <- execution_error_linear_fits %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      prediction = predict(model, data.frame(target_height_yards = height))
    ) %>%
    dplyr::pull(prediction)

  matrix(c(predictions[1], predictions[3], predictions[3], predictions[2]), nrow = 2)
}


#' Flip left-footed shots from Hunter et al. 2018
#'
#' Flip the horizontal error for left-footed shots
#'
#' @param data A data frame with each row representing one shot, with "foot" and
#' "horizontal_error_yards" columns
#'
#' @return The same data frame with the "horizontal_error_yards" column modified
flip_left_foot_Hunter_shots <- function(data)
{
  data %>%
    dplyr::mutate(
      horizontal_error_yards = ifelse(foot == "right", 1, -1) * horizontal_error_yards
    )
}
