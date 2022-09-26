# xcast a moosecrash likelihood given a version a specifed day or range of days
# produces 24 forecasts at 00h, 01h, .., 23h
#
# Calling Sequence:
#  Rscript /path/to/script /path/to/config/ date1 [date2] 
#  Rscript /mnt/ecocast/corecode/R/uvc/inst/scripts/v0.000-xcast-date.R /mnt/ecocast/projectdata/uvc/dvc/versions/v0/v0.000/v0.000.yaml 2022-09-16 2022-09-17
#
# see https://github.com/bd2kccd/r-causal/issues/14
options(java.parameters = "-Xmx1g" )

suppressPackageStartupMessages({
  library(dplyr)
  library(uvc)
  library(namforecast)
  library(dismo)
  library(dismotools)
  library(stars)
})

DEVMODE <- interactive()
if (DEVMODE){
  cfgfile <- '/mnt/ecocast/projectdata/uvc/dvc/versions/v0/v0.000/v0.000.yaml'
  #DATES <- format(as.Date("2022-09-16") + seq(from = -2, to = 2), "%Y-%m-%d") 
  DATES <- as.Date("2022-09-14")
} else {
  args <- commandArgs(trailingOnly = TRUE)
  cfgfile = args[1]
  DATES <- format(args[seq(from = 2, to = length(args))], "%Y-%m-%d")
} # check args

DATES <- as.Date(DATES, format = "%Y-%m-%d")
if (length(DATES) == 2) DATES <- seq(from = DATES[1], to = DATES[2], by = 'day')
DOYS <- format(DATES, "%j")
names(DATES) <- DOYS

# read the config and verify the output path
CFG <- uvc::read_configuration(cfgfile)
CFG$bbox <- sf::st_bbox(unlist(CFG$bbox), crs = 4326)
root <- uvc::build_version_path(CFG)
hours <- seq(from = 0, length = 24, by = CFG$predict$step)
HOURS <- sprintf( "%0.2i", hours)
names(hours) <- HOURS
TIMES <- lapply(seq_along(DATES),
                function(idate){
                  time <- sprintf("%s %s:00:00",
                                  format(DATES[idate], "%Y-%m-%d"),
                                  HOURS)
                  as.POSIXct(time, tz = "US/Eastern") |>
                    setNames(HOURS)
                })
names(TIMES) <- DOYS

MPATH   <- uvc::version_path(CFG, "model", DOYS)
XPATH   <- uvc::version_path(CFG, "xcast", DOYS)
IPATH   <- uvc::version_path(CFG, "image", DOYS)
names(MPATH) <- names(XPATH) <- names(IPATH) <- DOYS

MODELS <- sapply(DOYS,
 function(doy){
   path <- uvc::version_path(CFG, "model", doy)
   dismotools::read_maxent(path)
 }, simplify = FALSE)



# *must* have ROAD info
ROADS <- uvc::read_covariate_points(CFG)

TEMPLATE <- uvc::raster_template(ext = CFG$bbox)

# static road corvariates (is that an oxymoron?)
MEOGIS <- CFG$covariates$static$meogis
has_MEOGIS <- !is.null(MEOGIS)
if (has_MEOGIS){
  template <- stars::read_stars(file.path(MEOGIS$path, MEOGIS$template))
} else {
  template <- uvc::raster_template()
}

# do we have weather related stuff?
NAM <- CFG$covariates$dynamic$nam
has_NAM <- !is.null(NAM)
if (has_NAM){
  NAMPATH <- namforecast::nam_path("nwa", "forecast")
  NAMDB <- namforecast::read_database(NAMPATH) |>
    dplyr::mutate(trt_param = paste(.data$trt, .data$param, sep = "_"))
  nNAM <- length(NAM)
} # has_NAM

# solar stuff?
SOLAR <- CFG$covariates$dynamic$solar
has_SOLAR <- !is.null(SOLAR)


PREDS <- ROADS %>%
  dplyr::select(CFG$covariates$static$meogis$vars)


# PREDS = static predictors
# for (each day in DATES)
#   preds = PREDS + NAM
#   for (each hour in HOURS)
#     preds = preds + SOLAR
#     xcast = predict(model_for_doy, preds)
#     save xcast version_date_hour.tif
  

# A simple function to prepare the names of a NAM stars object
# from '20220914_mean_relhum.tif' to 'mean_relhum'
# @param object, such as stars, with name attribute
# @return the same object with new names
rename_NAM <- function(s){
  n <- gsub(".tif", "", basename(names(s)), fixed = TRUE)
  names(s) <- substring(n, regexpr("_", n) + 1)
  s
}

# @param hour character '01' 2-character hour
# @param doy charcater '257' julian day of year number
# @param preds sf
do_hour <- function(hour, doy, preds){
  
  sun <- dplyr::mutate(preds, datetime = TIMES[[doy]][[hour]]) |>
    uvc::get_solar_pos(datetime = "datetime") |>
    dplyr::select(dplyr::any_of(SOLAR)) 
  
  preds <- dplyr::bind_cols(preds, sun) |>
    na.omit()
  
  complete_set <- c(MEOGIS$vars, SOLAR, NAM)

  p <- dplyr::select(preds, dplyr::all_of(complete_set)) |>
    sf::st_drop_geometry()
  
   xcast <- preds |>
     dplyr::mutate(pred = dismo::predict(MODELS[[doy]], p)) |>
     dplyr::select(pred)
  
   
  if (grepl("raster",CFG$prediction$format, fixed = TRUE)){
    xfile <- uvc::version_path(CFG, "xcast", 
                               format(TIMES[[doy]][[hour]], "%Y-%m-%dT%H%M%S.tif"))
    cat("writing", xfile, "\n")
    stars::st_rasterize(xcast, template = TEMPLATE, file = xfile)
  }
   
  if (grepl("rds",CFG$prediction$format, fixed = TRUE)){
    xfile <- uvc::version_path(CFG, "xcast", 
                               format(TIMES[[doy]][[hour]], "%Y-%m-%dT%H%M%S.rds"))
    cat("writing", xfile, "\n")
    saveRDS(xcast, file = xfile)
  }   
  
   if (grepl("geopackage",CFG$prediction$format, fixed = TRUE)){
    xfile <- uvc::version_path(CFG, "xcast", 
                               format(TIMES[[doy]][[hour]], "%Y-%m-%dT%H%M%S.gpkg"))
    cat("writing", xfile, "\n")
    sf::write_sf(xcast, xfile, driver = 'GPKG', delete_dsn = TRUE)
  }   
   
  invisible(NULL)
}


# Prepare xcast for a given doy
# static variables are in PRED already
# 
if (DEVMODE) doy = '257'
do_doy <- function(doy){
  
  thisdate <- DATES[doy]
  
  if (has_NAM){
    namdb <- dplyr::filter(NAMDB, date %in% thisdate & trt_param %in% NAM)
    namff <- namforecast::compose_filename(namdb, NAMPATH)
    preds <- PREDS |>
      dplyr::bind_cols(stars::read_stars(namff) |>
                        rename_NAM() |>
                        stars::st_extract(at=ROADS) |>
                        sf::st_drop_geometry() )
  }
  
  if (has_SOLAR){
    xcasts <- sapply(HOURS, do_hour, doy, preds, simplify = FALSE)
  } else {
    # let's not worry about this just yet, it's quite awkward to not have at least some
    # sub-day predictors
  }
  
}