#' Get grid of truncated bivariate Gaussian components
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
#' @return Data frame with columns (y, z) indicating the component mean,
#' lambda indicating the associated lambda value, and cov indicating the
#' covariance matrix.
#'
#' @export
get_trunc_component_grid <- function(num_y = 11, num_z = 6, lambdas = c(1.0, 3.8)) {
  means_y <- seq(y_left_post(), y_right_post(), length.out = num_y + 2)[2:(num_y+1)]
  means_z <- seq(0, z_crossbar(), length.out = num_z + 2)[2:(num_z+1)]
  expand.grid(y = means_y, z = means_z, lambda = lambdas) |>
    dplyr::mutate(cov = lapply(z, get_execution_error_covariance)) |>
    dplyr::mutate(cov = purrr::map2(lambda, cov, ~ .x * .y)) |>
    tibble::tibble()
}
