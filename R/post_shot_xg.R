#' Filter shots for post-shot expected goals model
#'
#' @description Filter the shots data to include only shots used to train the
#' post-shot expected goals model.
#'
#' @param data A data frame with shot details
#'
#' @return The same data with only shots used to train the post-shot expected goals model.
#'
#' @export
filter_post_xg_shots <- function(data)
{
  data %>%
    dplyr::filter(is_on_target(y_end_proj, z_end_proj))
}


#' Predict post-shot expected goals
#'
#' @description Predict the probability of a goal given the shot details and
#' post-shot expected goals model.
#'
#' @param data A data frame with shot details
#' @param model A fit glm model used to predict post-shot expected goals values
#'
#' @return The same data with only shots used to train the post-shot expected goals model.
#'
#' @export
#'
#' @examples
#' predict_post_xg(y = 35:42, z = 0:3)
predict_post_xg <- function(y, z)
{
  shots <- data.frame(y_end_proj = y, z_end_proj = z)
  ifelse(
    is_on_target(shots$y_end_proj, shots$z_end_proj),
    predict(post_shot_xg_model, newdata = shots, type = "response"),
    0)
}
