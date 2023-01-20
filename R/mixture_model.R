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


#' Get shot probability densities
#'
#' Get a matrix with the density function for each shot and each component.
#'
#' @param mixture_model_components A data.frame with m rows and columns mean and
#' cov representing the mean vector and covariance matrix for each component.
#' @param shots An n x 2 matrix with the columns representing the y- and z-coordinates respecitvely.
#'
#' @return An n x m matrix with the (i, j) entry containing the probability density
#' function of the i'th shot for the j'th mixture model component.
#'
#' @export
get_shot_probability_densities <- function(mixture_model_components, shots) {
  # TODO Use apply instead of iteration

  pdfs <- matrix(nrow = nrow(shots), ncol = nrow(mixture_model_components))

  for (i in 1:nrow(mixture_model_components)) {
    pdfs[,i] <- tmvtnorm::dtmvnorm(
      as.matrix(shots),
      mean = mixture_model_components$mean[[i]],
      sigma = mixture_model_components$cov[[i]],
      lower = c(-Inf, 0)
    )
  }

  pdfs
}

#' Fit global component weights
#'
#' Given a matrix containing the probability density functions for each shot and
#' component pair, fit the mixture model weights of the component for the global
#' mixture model.
#'
#' @param pdfs An n x m matrix with the (i, j) entry containing the probability
#' density function of the i'th shot for the j'th mixture model component.
#' @param ... Arguments passed on to \link[rstan]{sampling}.
#'
#' @return A vector of length m indicating the mixture weights of each component.
#'
#' @export
fit_global_weights <- function(pdfs, ...) {
  standata <- list(
    num_shots = nrow(pdfs),
    num_components = ncol(pdfs),
    trunc_pdfs = pdfs
  )
  out <- rstan::sampling(stanmodels$global_mm_weights, data = standata, ...)

  out |>
    rstan::get_posterior_mean(pars = "global_weights") |>
    as.vector()
}

#' Fit player component weights
#'
#' Given a matrix containing the probability density functions for each shot and
#' component pair, fit the mixture model weights of the component for the global
#' mixture model.
#'
#' @param pdfs An n x k matrix with the (i, j) entry containing the probability
#' density function of the i'th shot for the j'th mixture model component.
#' @param player_labels An integer vector of length n indicating the player associated with
#' each shot. If there are p players, player_labels should contain only the
#' integers 1 through p.
#' @param global_weights A vector of length k indicating the weights of each
#' component for the global mixture model.
#' @param alpha A number representing the degree to which player weights are
#' shrunk towards the global weights. alpha = 0 corresponds to no shrinkage,
#' while alpha = Inf will force all players to have the global weights.
#'
#' @return A p x k matrix with the (i, j) entry corresponding to the weight of
#' the j'th component for player i.
#'
#' @export
fit_player_weights <- function(pdfs, player_labels, global_weights, alpha = 30) {
  if (alpha == Inf) {
    matrix(
      rep(global_weights, max(player_labels)),
      ncol = length(global_weights),
      byrow = TRUE
    )
  } else if (alpha == 0) {
    # TODO Implement
    stop("Not implemented yet. Use colMeans indexed for player's shots")
  } else {
    # TODO Implement
    stop("Not implemented yet. Call Stan")
  }
}
