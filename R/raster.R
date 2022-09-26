#' Make a raster template
#'
#' @export
#' @param ext sf "bbox" class object (default for Maine)
#' @param ncols number of columns
#' @param nrows number of rows
#' @param crs projection
#' @param value initial value to assign 
#' @return stars class object
raster_template <- function(
    ext = sf::st_bbox(c(xmin = -71.1, ymin = 43, xmax = -67, ymax = 47.5)),
    ncols = 200,
    nrows = 300,
    crs = 4326,
    value = NA_real_){
    stars::st_as_stars(ext,
                       nx = ncols[1],
                       ny = nrows[1],
                       values = value[1]) |>
    sf::st_set_crs(crs[1])
}

#' Rasterize data using the specified template
#'
#' @export
#' @param x sf object to rasterize
#' @param template the raster template
#' @param ... further arguments for \code{stars\link[stars]{st_rasterize}}
rasterize <- function(x, template = raster_template(), ...){

  stars::st_rasterize(x, template = template, ...)
}
