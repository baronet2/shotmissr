#' x-coordinate of goal line
x_goal_line <- function()
{
  120
}

#' y-coordinate of left post
left_post_y <- function()
{
  36
}

#' y-coordinate of right post
right_post_y <- function()
{
  44
}

#' z-coordinate of crossbar
crossbar_z <- function()
{
  2.67
}

#' Is shot on target?
#'
#' @param y A numeric vector of shot end y coordinates
#' @param z A numeric vector of shot end z coordinates#'
#'
#' @return Boolean vector indicating whether each shot is on target
#'
#' @export
is_on_target <- function(y, z)
{
  (y >= left_post_y()) & (y <= right_post_y()) & (z <= crossbar_z())
}
