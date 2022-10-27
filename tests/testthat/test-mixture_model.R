testthat::test_that("get_trunc_component_grid output correct", {
  num_y = 11
  num_z = 6
  lambdas = c(1.0, 3.8)

  out <- get_trunc_component_grid(num_y, num_z, lambdas)

  testthat::expect_equal(
    out$cov[[1]] / lambdas[1],
    out$cov[[1 + num_y * num_z]] / lambdas[2]
  )

})
