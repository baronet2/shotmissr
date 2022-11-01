#' Get bivariate Gaussian mixture model components
#'
#' The means of the components are evenly-spaced points along a grid within the goal frame.
#' The covariance matrices of the components are based on empirical observations of
#' the shape of execution errors for different shot target heights.
#' There are multiple layers of components, with each layer having a different scaling factor lambda.
#'
#' @param num_y The number of grid points in the y direction
#' @param num_z The number of grid points in the z direction
#' @param lambdas A numeric vector containing the scaling factors for each layer
#'
#' @return Data frame with columns mean, lambda, and cov indicating the component
#' mean, associated lambda value for scaling, and (scaled) covariance matrix.
#'
#' @export
get_mixture_model_components <- function(num_y = 11, num_z = 6, lambdas = c(1.0, 3.8)) {
  means_y <- seq(y_left_post(), y_right_post(), length.out = num_y + 2)[2:(num_y+1)]
  means_z <- seq(0, z_crossbar(), length.out = num_z + 2)[2:(num_z+1)]
  expand.grid(y = means_y, z = means_z, lambda = lambdas) |>
    dplyr::mutate(cov = lapply(z, get_execution_error_covariance)) |>
    dplyr::mutate(cov = purrr::map2(lambda, cov, ~ .x * .y)) |>
    dplyr::mutate(mean = purrr::map2(y, z, ~ c(.x, .y))) |>
    dplyr::select(mean, lambda, cov)
}


#' Get component values
#'
#' Get the expected value of each truncated bivariate Gaussian distribution,
#' computed using Monte Carlo simulation.
#'
#' @param mixture_model_components A data.frame with columns mean and cov representing
#' the mean vector and covariance matrix for each component.
#' @param n_samples The number of samples from each component used for Monte Carlo integration
#'
#' @return The same data frame with new column "value" indicating the mean post-shot
#' expected goals value for a shot samples from that component.
#'
#' @export
get_component_values <- function(mixture_model_components, n_samples = 100) {
  mixture_model_components |>
    dplyr::mutate(
      samples = purrr::map2(
        mean, cov,
        ~ tmvtnorm::rtmvnorm(
          n = n_samples,
          mean = .x,
          sigma = .y,
          lower = c(-Inf, 0)
        ),
      ),
      values = purrr::map(
        samples,
        ~ predict_post_xg(.[,1], .[,2])
      ),
      value = sapply(values, mean)
    ) |>
    dplyr::select(-samples, -values)
}

