#' Load RBPostXg
#'
#' Adds a column `rb_post_xg` to a data frame of shots given the component weights
#' and mixture model components.
#'
#' @param shooting_skill_data A data frame with shot details and a `group_id`
#' column indicating the grouping level at which player weights were fit.
#' @param player_weights A p x k matrix with each row corresponding to the mixture
#' model component weights associated with group p. Output from \link{fit_player_weights}.
#' @param mixture_model_components A data frame with column `value` indicating the
#' value of each component of the mixture model. Ordered the same as `player_weights`.
#' Output from \link{get_mixture_model_components}.
#'
#' @return The same shots data frame with a new column `rb_post_xg`. The `rb_post_xg`
#' values will be identical for any rows with identical `group_id`.
#'
#' @export
load_rb_post_xg <- function(shooting_skill_data, player_weights, mixture_model_components) {
  rb_post_xg <- data.frame(
    group_id = 1:max(shooting_skill_data$group_id),
    rb_post_xg = player_weights %*% mixture_model_components$value
  )

  shooting_skill_data |>
    dplyr::inner_join(rb_post_xg, by = "group_id")
}

#' Load GenPostXg
#'
#' Adds a column `gen_post_xg` to a data frame of shots given the component weights
#' and values.
#'
#' @param shooting_skill_data A data frame with shot details and a `group_id`
#' column indicating the grouping level at which player weights were fit.
#' @param pdfs Output from \link{get_shot_probability_densities}.
#' @param component_weights Vector of length k adding up to 1, indicating the
#' mixture model weights for each component.
#' @param component_values Vector of length k indicating the value of each
#' mixture model component.
#'
#' @return The same shots data frame with a new column `gen_post_xg`.
#'
#' @export
load_gen_post_xg <- function(shooting_skill_data, pdfs, component_weights, component_values) {
  # Multiply pdf for each component by its weight
  weighted_pdfs <- t(t(pdfs) * component_weights)
  # Normalize each row so each shot has cumulative probability of 1
  component_probs <- t(apply(weighted_pdfs, 1, function(row) row / sum(row)))
  # Add column with dot product of component probabilities and their weights
  shooting_skill_data |>
    dplyr::mutate(gen_post_xg = component_probs %*% component_values)
}
