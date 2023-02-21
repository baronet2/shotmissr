testthat::test_that("get_mixture_model_components output correct", {
  num_y = 11
  num_z = 6
  lambdas = c(1.0, 3.8)

  out <- get_mixture_model_components(num_y, num_z, lambdas)

  testthat::expect_equal(
    out$cov[[1]] / lambdas[1],
    out$cov[[1 + num_y * num_z]] / lambdas[2]
  )

})

testthat::test_that("fit_player_weights corrrect for select alphas", {
  pdfs = matrix(c(0.5, 0.5, 1,
                  0, 1.5, 0.5),
                ncol = 3,
                byrow = TRUE)
  global_weights <- c(0.3, 0.4, 0.3)
  inf_result <- fit_player_weights(pdfs, 1:2, alpha = Inf, global_weights)
  testthat::expect_equal(inf_result$global_weights, global_weights)
  apply(inf_result$player_weights, 1, function(row) testthat::expect_equal(row, global_weights))

  zero_result <- fit_player_weights(pdfs, 1:2, alpha = 0, global_weights)
  testthat::expect_equal(zero_result$global_weights, c(0.25, 1, 0.75))
  testthat::expect_equal(
    zero_result$player_weights,
    matrix(c(0.25, 0.25, 0.5, 0, 0.75, 0.25), nrow = 2, byrow = TRUE))
})
