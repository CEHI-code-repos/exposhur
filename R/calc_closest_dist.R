#' Calculate Closest Distance and Time
#'
#' @description
#' Calculates at what time point the storm is closest and records the distance
#' from track at that time.
#'
#' @param hurdat2 Output from `get_hurdat2()` or interpolated track from 
#' `calc_interpolated_track()`
#' @param geography `sf` object for the area of interest
#' @param geoid Unique identifier for geography
#'
#' @return Closest distance and time to track for each geography
#' @export
#'
#' @examples
#' \dontrun{
#' harvey_closest_dist <- calc_closest_dist(
#'   harvey_hurdat2,
#'   TXLA_tracts
#' )
#' }
calc_closest_dist <- function(track, geography, geoid = "GEOID") {
  geography |>
    dplyr::mutate(
      "near_idx" = sf::st_nearest_feature(geography, track),
      "closest_date" = track$datetime[.data$near_idx],
      "closest_dist" = sf::st_distance(
        track$geometry[.data$near_idx],
        geography,
        by_element = TRUE
      ),
      "closest_dist" = units::set_units(.data$closest_dist, "km")
    ) |>
    sf::st_drop_geometry() |>
    tibble::tibble() |>
    dplyr::select(c("geoid" = geoid, "closest_dist", "closest_date"))
}

#' Calculate Interpolated Track
#'
#' @description
#' Utilizes [`stormwindmodel`](https://github.com/geanders/stormwindmodel)
#' to impute the `hurdat2` track to 15 minute intervals.
#'
#' @param track Output from `get_hurdat2()`
#'
#' @return Interpolated track at 15 minute intervals
#' @export
calc_interpolated_track <- function(track) {
  track <- track |>
    dplyr::mutate(
      lon = purrr::map_dbl(sf::st_geometry(track), 1),
      lat = purrr::map_dbl(sf::st_geometry(track), 2)
    )

  track |>
    dplyr::rename(longitude = lon, latitude = lat, date = datetime) |>
    dplyr::mutate(date = format(date, "%Y%m%d%H%M")) |>
    stormwindmodel::create_full_track()  |>
    sf::st_as_sf(coords = c("tclon", "tclat"), crs = 4326, na.fail = FALSE) |>
    dplyr::rename(datetime = date)
}
