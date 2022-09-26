#' Construct a version path
#' 
#' @export
#' @param cfg configuration list
#' @param ... element passed to \code{file.path}
#' @param create logical, if TRUE create the path if it doesn't exist.  Only 
#'   meaningful if you are creating a directory.  Note this follows \code{...}
#' @return file path specification
version_path <- function(cfg, ..., create = FALSE){
  v <- parse_version(cfg$version)
  path <- file.path(cfg$datapath, "versions", v[['major']], cfg$version, ...)
  if (create[1]){
    if (!dir.exists(path)) ok <- dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  path
}


#' Parse a version string
#'
#' Versions have the format vMajor.Minor.Release
#'
#' @export
#' @param x version string to parse
#' @return named character vector
parse_version <- function(x = 'v2.123.1'){
    xx = strsplit(x, '.', fixed = TRUE)[[1]]
    c(major = xx[1], minor = xx[2], release = ifelse(length(xx) == 3, xx[3], NA))
}

#' Compose a version string from components
#'
#' Versions have the format vMajor.Minor.Release
#'
#' @export
#' @param major string, like 'v3' or a named vector with (major, minor, [release])
#' @param minor string, like '13', ignored if major has length > 1
#' @param release string, optional like '002', ignored if major has length > 1
#' @return version string like 'v3.13.002'
compose_version <- function(major, minor, release){
    if(missing(major)) stop("major is required")
    if (length(major) >= 2){
        v = paste(major, collapse = '.')
    } else {
        if(missing(minor)) stop("minor is required")
            v = paste(major, minor[1], sep  = '.')
        if(!missing(release)) v = paste(v, release[1], sep  = '.')
    }
    v
}


#' Build a version output structure, or if existing then check it
#' 
#' @export
#' @param cfg configuration list
#' @param subdirs character vector of subdirectories to create
#' @return the root path for the version
build_version_path <- function(cfg,
                               subdirs = c("model", "xcast", "image")){
 
  root <- version_path(cfg)
  if (!file.exists(root)) ok <- dir.create(root, recursive = TRUE)
  ok <- sapply(subdirs, 
               function(subdir){
                 path <- file.path(root, subdir)
                 if (!dir.exists(path)) dir.create(path, recursive = TRUE)
               })
  root
}
