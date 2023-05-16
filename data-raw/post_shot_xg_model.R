# From https://win-vector.com/2014/05/30/trimming-the-fat-from-glm-models-in-r/
stripGlmLR = function(cm) {
  cm$y = c()
  cm$model = c()

  cm$residuals = c()
  cm$fitted.values = c()
  cm$effects = c()
  cm$qr$qr = c()
  cm$linear.predictors = c()
  cm$weights = c()
  cm$prior.weights = c()
  cm$data = c()


  cm$family$variance = c()
  cm$family$dev.resids = c()
  cm$family$aic = c()
  cm$family$validmu = c()
  cm$family$simulate = c()
  attr(cm$terms,".Environment") = c()
  attr(cm$formula,".Environment") = c()

  cm
}

data <- statsbomb_shots_processed %>%
  dplyr::mutate(Goal = outcome == "Goal")

post_shot_xg_model <- glm(Goal ~ stats::poly(y_end_proj, 3) + stats::poly(z_end_proj, 3), data = data, family = "binomial") %>%
  stripGlmLR()

usethis::use_data(post_shot_xg_model, overwrite = TRUE)
