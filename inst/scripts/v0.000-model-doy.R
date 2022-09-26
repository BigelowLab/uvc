# given a config file generate one or more models for the given days
#
# Calling Sequence:
#  Rscript /path/to/script /path/to/config/ doy1 [doy2] 
#  Rscript /mnt/ecocast/corecode/R/uvc/inst/scripts/v0.000-model-doy.R /mnt/ecocast/projectdata/uvc/mvc/versions/v0/v0.000/v0.000.yaml 257-262
#
# doy may be a single '077' or a range of doys '070-093'. Note three character representation.

suppressPackageStartupMessages({
  library(dplyr)
  library(sf)
  library(stars)
  library(uvc)
  library(dismo)
  library(dismotools)
})


DEVMODE <- interactive()
if (DEVMODE){
  cfgfile <- '/mnt/ecocast/projectdata/uvc/dvc/versions/v0/v0.000/v0.000.yaml'
  DOYS <- format(Sys.Date() + seq(from = -2, to = 2), "%j") 
} else {
  args <- commandArgs(trailingOnly = TRUE)
  cfgfile = args[1]
  doys <- args[seq(from = 2, to = length(args))]
  if (length(doys) == 1){
    DOYS <- sprintf("%0.3i", as.integer(doys))
  } else {
    DOYS <- sprintf("%0.3i", 
                    seq(from = as.integer(doys[1]),
                        to = as.integer(doys[2]),
                        by = 1))
      
  }
} # check args

# read the config and verify the output path
CFG <- uvc::read_configuration(cfgfile)
CFG$bbox <- sf::st_bbox(xmin = CFG$bbox[1], ymin = CFG$bbox[2],
                        xmax = CFG$bbox[3], xmax = CFG$bbox[4])
root <- uvc::build_version_path(CFG)


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
  
  # subroutine to extract one or more variables for a given date
  # Use with group_map where items are grouped by day
  # @param tbl sf tibble, must have trt_param and date columns
  # @param key ignored
  # @param verbose logival, for debugging
  # @return updated tibble (more columns for NAM) or NULL
  extract_nam <- function(tbl, key, verbose = FALSE){
    if(verbose) print(tbl$date[1])
    namdb <- dplyr::filter(NAMDB,
                           date == tbl$date[1] &
                             trt_param %in% NAM)
    if (nrow(namdb) == nNAM){
      ss <- namforecast::read_nam(namdb, NAMPATH)
      names(ss) <- namdb$trt_param
      r <- dplyr::bind_cols(tbl, sf::st_drop_geometry(stars::st_extract(ss, tbl)))
    } else {
      r <- NULL
    }
    r
  }
  
} # has_NAM

# solar stuff?
SOLAR <- CFG$covariates$dynamic$solar
has_SOLAR <- !is.null(SOLAR)

# *must* have ROAD info
ROADS <- uvc::read_covariate_points(CFG)

# read in the obs - remap where they misalign with roads
CRASHES <- uvc::read_occurrence(CFG, template = template) |>
  uvc::relocate_occurrences(ROADS)

if (DEVMODE) doy <- DOYS[1]

# this is the primary function that accepts one DOY, all others variables are mined
# from the enclosing environment
# @param doy charcater doy
# @return tibble of summaries
model_doy <- function(doy){
  
  doys <- uvc::doy_window(doy, w = CFG$model$window, MAX = 365, form = 'character')
  crashes <- CRASHES %>%
    dplyr::filter(doy %in% doys)
  if (nrow(crashes) < CFG$model$nmin){
    warning("this doy", doy, "has too few occurences")
    return(NULL)
  }
  
  yr <- range(crashes$date)

  # first the presence points - collect covariates
  
  # obs are the crashes colocated with ROADS for this DOY
  # then we add variables from MEOGIS, SOLAR and NAM
  roads <- dplyr::slice(ROADS, match(crashes$cell, cell))
  #
  if (has_MEOGIS) {
    roads <- roads |>
      dplyr::select(dplyr::any_of(c("date", MEOGIS$vars)))
  }
  
  obs <- crashes |>
    dplyr::bind_cols(roads |>
                       sf::st_drop_geometry())
  
  if (has_SOLAR){
    obs <- obs |>
      dplyr::bind_cols(uvc::get_solar_pos(obs, datetime = "datetime") |>
                         dplyr::select(dplyr::all_of(SOLAR)))
  }
  
  if (has_NAM){
    # we end up with a reduced dataset *if* any weather data layers are missing
    # in the database (it happens!) so nrow(obs_in) >= nrow(obs_out)
    obs <- dplyr::group_by(obs, date) |>
      dplyr::group_map(extract_nam, .keep = TRUE) |>
      dplyr::bind_rows()
  }
  
  # characterize background 
  nBACK <- CFG$model$nback
  bak <- ROADS %>%
    dplyr::filter(!(cell %in% crashes$cell)) %>%
    dplyr::slice(sample(n(), nBACK, replace = FALSE)) %>%
    dplyr::mutate(datetime = random_datetime(r = yr, N = nBACK, doy = doys),
                  date = as.Date(datetime))
  
  
  if (has_SOLAR){
    bak <- bak |>
      dplyr::bind_cols(uvc::get_solar_pos(bak, datetime = "datetime") |>
                         dplyr::select(dplyr::all_of(SOLAR)))
  }
  
  if (has_NAM){
    # we end up with a reduced dataset *if* any weather data layers are missing
    # in the database (it happens!) so nrow(obs_in) >= nrow(obs_out)
    bak <- dplyr::group_by(bak, date) |>
      dplyr::group_map(extract_nam, .keep = TRUE, verbose = DEVMODE) |>
      dplyr::bind_rows()
  }
  
  complete_set <- c(MEOGIS$vars, SOLAR, NAM)
  obs <- dplyr::select(obs, dplyr::all_of(complete_set)) |>
    sf::st_drop_geometry()
  bak <- dplyr::select(bak, dplyr::all_of(complete_set)) |>
    sf::st_drop_geometry()
  
  model_path <- uvc::version_path(CFG, "model", doy)
  if (!dir.exists(model_path)) ok <- dir.create(model_path, recursive = TRUE)
  flag <- c(rep(1, nrow(obs)), rep(0, nrow(bak)))
  model <- dismo::maxent(dplyr::bind_rows(obs, bak), flag, path = model_path)
  dismotools::maxent_summary(model, fmt = 'tibble', save_summary = TRUE)
}

ofile <- uvc::version_path(CFG, format(Sys.time(), "%Y-%m-%dT%H%M%S-summary.csv"))
rr <- lapply(DOYS, model_doy) |>
  dplyr::bind_rows() |>
  readr::write_csv(ofile)
