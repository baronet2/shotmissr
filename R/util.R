#' x-coordinate of goal line
x_goal_line <- function()
{
  120
}

#' y-coordinate of left post
y_left_post <- function()
{
  36
}

#' y-coordinate of right post
y_right_post <- function()
{
  44
}

#' y-coordinate of center line
y_center_line <- function()
{
  (y_left_post() + y_right_post()) / 2
}

#' z-coordinate of crossbar
z_crossbar <- function()
{
  2.67
}

#' Is shot on target?
#'
#' @param y A numeric vector of shot end y coordinates
#' @param z A numeric vector of shot end z coordinates
#'
#' @return Boolean vector indicating whether each shot is on target
#'
#' @export
is_on_target <- function(y, z)
{
  (y >= y_left_post()) & (y <= y_right_post()) & (z <= z_crossbar())
}
