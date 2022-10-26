#' Plot goal posts
#'
#' Add line segments showing the goal posts and crossbar to a ggplot.
#'
#' @param ... Arguments passed to geom_segment
#'
#' @return ggplot2::geom_segment with both posts and crossbar
#'
#' @export
plot_goalposts <- function(...)
{
  ggplot2::geom_segment(
    data = data.frame(
      x = c(y_left_post(), y_left_post(), y_right_post()),
      x_end = c(y_left_post(), y_right_post(), y_right_post()),
      y = c(0, z_crossbar(), z_crossbar()),
      y_end = c(z_crossbar(), z_crossbar(), 0)
    ),
    mapping = ggplot2::aes(x = x, y = y, xend = x_end, yend = y_end),
    ...
  )
}
