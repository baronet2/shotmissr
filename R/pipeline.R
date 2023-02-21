#' Get player groups
#'
#' Add a column indicating the index of the group each shot belongs to,
#' and filter to groups meeting a specified threshold.
#'
#' @param shots_data A dataframe with one row per shot
#' @param grouping_cols A character vector of columns in shots_data, indicating
#' the level(s) at which to fit mixture model components. Typically, this will be
#' at the player, player-season, or player-half-season level.
#' @param group_size_threshold An integer indicating the minimum group size to be included.
#'
#' @return shots_data with two modifications. First, a new column group_id is
#' added, containing integers from 1 to some number k identifying the group for each shot.
#' Second, the dataframe is filtered to only groups meeting the specified threshold.
#'
#' @export
get_player_groups <- function(shots_data, grouping_cols = c("player"), group_size_threshold = 30) {
  shots_data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping_cols))) |>
    dplyr::filter(dplyr::n() >= group_size_threshold) |>
    dplyr::mutate(group_id = dplyr::cur_group_id()) |>
    dplyr::ungroup()
}
