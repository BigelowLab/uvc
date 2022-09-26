#' Decompose solar azimuth into u and v directions
#'
#' @export
#' @param x a sequence of azimuth values
#' @return a tibble contains u and v direction values
azimuth_to_uv <- function(x = seq(from = 0, to = 360, by = 1)){
  rx = (x-90) * 2 * pi / 360
  ry = (x+90) * 2 * pi / 360
  u = cos(rx)
  v = sin(ry)
  y = dplyr::tibble(u = u, v = v)
  return(y)
}


#' Compute the solar position in azimuth and solar elevation for given long, lat, and date_time
#'
#' @export
#' @param x location (likely lon) or sf object.  If the latter then \code{y} and 
#'  \code{datetime} are ignored and x will be searched for a POSIX column.
#' @param y location (likely lat) 
#' @param datetime datetime as POSIXct
#' @return a tibble contains solar azimuth u and v directions and solar elevation
get_solar_pos <- function(x = -135, y = 44.5, datetime = Sys.time()){
  
  if (inherits(x, 'sf')){
    if (inherits(datetime, 'character')) {
      datetime <- x[[datetime[1]]]
    } else {
      klass <- which(sapply(x, function(x) inherits(x, 'POSIXct')))
      if (length(klass) == 0) stop("if x in sf object it must has a POSIX column")
      datetime <- x[[klass[1]]]
    }
    xy <- sf::st_coordinates(x)
    x <- xy[,1]
    y <- xy[,2]
  }
  
  spos <- suncalc::getSunlightPosition(data = data.frame(date = datetime,
                                                 lat = y,
                                                 lon = x),
                               keep = c("altitude", "azimuth")) |>
    dplyr::mutate(solar_elevation = .data$altitude * 180/pi, .after = .data$altitude) |>
    dplyr::mutate(dazimuth = ((.data$azimuth-pi)*180/pi) %% 360) |>
    dplyr::as_tibble()

    dplyr::bind_cols(spos, azimuth_to_uv(spos$dazimuth)) |>
    dplyr::select(solar_elevation,
                  solar_azimuth_u = .data$u,
                  solar_azimuth_v = .data$v)

}
