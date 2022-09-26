#' Read occurence data
#'
#' @export
#' @param cfg configuration list
#' @param add character vector of one or more variable to add.
#'  \itemize{
#'    \item{cell add the cell number relative to a template raster}
#'    \item{datetime add POSIXct combination of date and hour}
#'    \item{doy add 3-character day of year}
#'  }
#' @param ... other arguments passed to \code{\link{raster_template}}
#' @param template NULL or stars object, for adding cell
#' @return sf POINT object
read_occurrence <- function(cfg,
    add = c("cell", "datetime", "doy"),
    ...,
    template = if(length(add) > 0) raster_template(...) else NULL){
  
    filename <- cfg$occur$filename
  
    x = readr::read_csv(filename[1], show_col_types = FALSE) |>
      sf::st_as_sf(coords = c("LONG", "LAT"), crs = 4326)
    colnames(x) <- tolower(colnames(x))

    
    if ("datetime" %in% add){
      dt <- paste(format(x$date, "%Y-%m-%d"), paste0(x$hour, ":00:00"))
      x <- dplyr::mutate(x, 
                         datetime = as.POSIXct(dt, tz = 'EDT'))
    }
    if ("doy" %in% add){
      x <- dplyr::mutate(x, 
                         doy = format(.data$date, "%j"))
    }
    if ("cell" %in% add) {
      if (is.null(template)) template = raster_template(...)
      x <- dplyr::mutate(x, 
                         cell = stars::st_cells(template, x), .before = 1)
    }
    
    return(x)
}

#' Read covariate point data
#'
#' @export
#' @param cfg configuration list
#' @return sf object
read_covariate_points <- function(cfg){
  readr::read_csv(cfg$covariates$static$roads$filename, show_col_types = FALSE) |>
    sf::st_as_sf(coords = c("long", "lat"), crs = 4326)
}

#' Read the static covariate layers - likely read as proxy (virtual) object
#' 
#' @export
#' @param cfg configuration  list
#' @return stars object
read_covariate_static_layers <- function(cfg){
  
  ff <- list.files(cfg$covariates$static$meogis$path, 
                   pattern = "^.*_longlat\\.tif$", 
                   full.names = TRUE)
  r = stars::read_stars(ff)
  names(r) <- sub( "_longlat.tif", "", basename(names(r)), fixed = TRUE)
  r
}


#' Read a suite of xcast rasters by date
#'
#'
#' @export
#' @param cfg configuration list
#' @param when Date object of one or more dates
#'    can't be found, then the most recent on that day is returned
#' @return either RasterStack or NULL
read_xcast_date <- function(cfg, when = Sys.Date()){

    if (!inherits(when, 'Date')) {
        w <- try(as.Date(when))
        if (inherits(w, "try-error")) stop("unable to convert when to Date class:", when)
        when <- w
    }

    doy     <- format(when, "%j")
    ff <- list.files(version_path(cfg, "xcast", doy),
            pattern = utils::glob2rx("*.tif"), full.names = TRUE)
    if (length(ff) == 0) return(NULL)
    stars::read_stars(ff) |> split()
}


#' Read an xcast as a table or raster
#'
#' @export
#' @param cfg configuration list
#' @param when POSIXct timestamp  If the xcast exactly matching the requested hour
#'    can't be found, then the most recent on that day is returned
#' @param version the moosecrash version
#' @param form character either "points" (or "tibble") or "raster"
#' @return either a tibble, raster or NULL
read_xcast <- function(cfg,
    when = Sys.time(),
    version = "v0.000",
    form = c("points", "raster", "tibble")[2]){

    form    <- tolower(form[1])
    doy     <- format(when, "%j")
    stamp   <- format(when, "%Y-%m-%-d-%H")
    pat     <- switch(form,
        "raster" = "*.tif",
        "*.csv.gz")


    ff <- list.files(version_path(cfg, "xcast", doy),
            pattern = utils::glob2rx(pat), full.names = TRUE)
    if (length(ff) == 0) return(NULL)
    ix <- grepl(stamp, basename(ff), fixed = TRUE)
    if (!any(ix)) ix[length(ix)] <- TRUE  # the most recent...

    switch(form,
        "raster" = stars::read_stars(ff[ix][1]),
        readr::read_csv(ff[ix][1])
        )
}
