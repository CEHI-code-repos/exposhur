#' Calculate Daily Average for Each Geography
#'
#' @description
#' Calculates a daily average for all `nClimGrid` measures
#' for each geography utilizing the `exactextractr` package
#' for zonal means.
#'
#' @param nClimGrid Output from `get_nClimGrid()`
#' @param geography `sf` object for the area of interest
#' @param geoid Unique identifier for geography
#'
#' @return Daily average for all `nClimGrid` measures for each
#' geography
#' @export
#'
#' @examples
#' \dontrun{
#' harvey_daily_avgs <- calc_daily_avgs(
#'   harvey_nClimGrid,
#'   TXLA_tracts
#' )
#' }
calc_daily_avgs <- function(nClimGrid, geography, geoid = "GEOID") {
  purrr::map(terra::time(nClimGrid), function(day) {
    nClimGrid_day <- nClimGrid[[terra::time(nClimGrid) == day]]

  zonal::execute_zonal(nClimGrid_day, geography, ID = geoid, fun = "mean", join = FALSE) |>
    dplyr::mutate(
      "date" = lubridate::as_date(day)
    ) |>
    dplyr::rename_with(
      .fn = \(x) str_remove(x, "_.*$"),
      .cols = starts_with("mean")
    )
  }) |>
    dplyr::bind_rows() |>
    dplyr::relocate(date, mean_prcp = mean.prcp, .after = dplyr::everything())
}
