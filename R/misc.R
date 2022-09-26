#' Reassign occurence locations to match road locations
#' 
#' First we compare cell locations. If any don't match a road cell then we
#' find the closest raod cell - imperfect but probably most logical.
#' 
#' @export
#' @param cr sf object - presumably crash occurences
#' @param rd sf object - presumably road covariate data
#' @return possibly updated version of input \code{cr}
relocate_occurrences <- function(cr, rd){
  ix <- which(!(cr$cell %in% rd$cell))
  if (length(ix) == 0) return(cr)
 
  d <- sf::st_distance(dplyr::slice(cr, ix), rd)
  imin <- apply(d, 1, which.min)
  cr$cell[ix] <- imin
  return(cr)
}




#' Parse and group parameters
#'
#' @export
#' @param x a vector of parameters
#' @return named list of parameter groupings
parse_and_group_params <- function(
    x = c("elevation", "forest_distance", "moose_harvest", "road_type",
        "slope_degree", "speed_limit", "aspect_u", "aspect_v", "water_distance",
        "wetland_distance", "traffic_volume", "road_density",
        "visibility", "cloud_cover", "airtemp", "solar_elevation",
        "solar_azimuth_u", "solar_azimuth_v")){


    ix <- rep("meogis", length(x))
    nam <- c("mean_vis", "mean_cloud", "mean_airtemp", "min_airtemp", "max_airtemp",
             "mean_relhum", "mean_sndep", "mean_trnstr", "mean_vegcvr", "first_vegtyp",
             "mean_sncvr", "mean_uwind", "mean_vwind", "mean_wilt", "sum_precip")
    meogis <- c("elevation", "forest_distance", "moose_harvest", "road_type",
                "slope_degree", "speed_limit", "aspect_u", "aspect_v", "water_distance",
                "wetland_distance", "traffic_volume", "road_density")
    solar <- "solar_"

    ix[mgrepl(nam, x, fixed = TRUE)] <- "nam"

    ix[mgrepl(solar, x, fixed = TRUE)] <- "solar"

    return(split(x, ix))
}



#' Generate random datetime as POSIXct within a range of dates
#'
#' @export
#' @param r a time range
#' @param N number of random selections
#' @param doy if provided limit selection candidates to these doys (001, 002, ..., 365, 366)
#' @return randomly selected POSIXct vector within the specifed range
random_datetime <- function(
    r = as.POSIXct(c("2006-01-01 00:00:00", "2010-12-31 23:00:00"), tz = "US/Eastern"),
    N = 1000,
    doy = NULL){
        if (!is.null(doy)){
            Years = as.numeric(format(r, '%Y'))
            Y <- seq(from = Years[1], to = Years[2], by = 1)
            dd <- as.vector(sapply(Y, function(y) sprintf("%0.4i%0.3i", y, as.numeric(doy))))
            dd <- as.Date(dd, format = '%Y%j')
        } else {
            d = as.Date(r)
            dd = seq(from = d[1], to = d[2], by = "day")

        }
        days = format(sample(dd, size = N, replace = TRUE), "%Y-%m-%d")

        hh = seq(from = 0, to = 23, by = 1)
        xx = seq(from = 0, to = 59, by = 1)
        times = sprintf("%0.2i:%0.2i:%0.2i",
              sample(hh, size = N, replace = TRUE),
              sample(xx, size = N, replace = TRUE),
              sample(xx, size = N, replace = TRUE))

        dt = paste(days, times)
        datetime = as.POSIXct(dt, format = "%Y-%m-%d %H:%M:%S", tz = "US/Eastern")

        return(datetime)
}


#' Compute the doy window given the day of year and window size
#'
#' Given doy 100 with a window of c(-5,5) will yield -95,-96,...104,105
#'
#' @export
#' @param x the day of year (should be 1-366)
#' @param w the window as 2 elements [days before, days after]
#' @param MAX the highest possible day number
#' @param form character, one of 'numeric' or 'character' to specify output
#' @return numeric doy vector
doy_window <- function(x = 1, w = c(-5,5), MAX = 366,
                       form = c("numeric", "character")[2]){
    newday <- as.numeric(x) + seq(from = w[1], to = w[2])
    ix <- newday < 1
    if (any(ix)) newday[ix] = MAX + newday[ix]
    ix <- newday > MAX
    if (any(ix)) newday[ix] = newday[ix] - MAX
    switch(tolower(form[1]),
           'character' = sprintf("%0.3i", newday),
           newday)
}



#' Perform grepl on multiple patterns; it's like  AND-ing or OR-ing successive grepl statements.
#' 
#' Adapted from https://stat.ethz.ch/pipermail/r-help/2012-June/316441.html
#'
#' @param pattern character vector of patterns
#' @param x the character vector to search
#' @param op logical vector operator back quoted, defaults to `|`
#' @param ... further arguments for \code{grepl} like \code{fixed} etc.
#' @return logical vector
mgrepl <- function(pattern, x, op = `|`, ... ){
  Reduce(op, lapply(pattern, grepl, x, ...))
}


#' Compute the union of a list of vectors
#'
#' @export
#' @param x a list of zero or more vectors
#' @return a vector of the union or NULL is the input is empty
munion <- function(x){
    if (!is.list(x)) stop("input must be a list")
    n <- length(x)
    if (n == 0) return(NULL)
    if (n == 1) return(x[[1]])

    s <- x[[1]]
    for (i in 2:n) s <- union(s, x[[i]])
    s
}

#' Compute the intersection of a list of vectors
#'
#' @export
#' @param x a list of zero or more vectors
#' @return a vector of the intersection or NULL is the input is empty
mintersect <- function(x){
    if (!is.list(x)) stop("input must be a list")
    n <- length(x)
    if (n == 0) return(NULL)
    if (n == 1) return(x[[1]])

    s <- x[[1]]
    for (i in 2:n) s <- intersect(s, x[[i]])
    s
}
