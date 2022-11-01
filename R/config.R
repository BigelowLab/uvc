#' Read a configuration (yaml) by version number
#'
#' @export
#' @param filename character, the file to read
#' @param autopopulate logical, if TRUE then scan for patterns such as \code{PATH}
#'   to use for autopopulating
#' @param patterns character, one or more string patterns to search for.  These might be 
#'   subsequently seeded throughout the remainder of the YAML as $pattern. Only the
#'   first occurence of each pattern is honored
#' @return the configuration as a named list
read_configuration <- function(filename, 
                               autopopulate = TRUE, 
                               patterns = "datapath"){
  
  x <- yaml::read_yaml(filename)
  if (autopopulate){
    s <- readLines(filename[1])
    for (pattern in patterns){
      ipattern <- grep(pattern, s, fixed = TRUE)
      value <- strsplit(s[ipattern[1]], ":", fixed = TRUE)[[1]][2]
      if (length(ipattern) > 0){
        s <- sub(paste0("$", pattern), value, s, fixed = TRUE)
      }
    }
    x <- yaml::read_yaml(text=paste(s, collapse = "\n"))
    
    if ("bbox" %in% names(x)){
      x$bbox <- unlist(x$bbox)
    }
    
  }

  x
}

#' Write a configuration (yaml)
#'
#' @export
#' @param x the configuration as a named list
#' @param filename charcater, the file specification to write to
#' @return the configuration as a named list
write_configuration <- function(x, filename){
  
  if ("bbox" %in% names(x)){
    y <- x
    if (inherits(x$bbox, "bbox")) x$bbox <- as.list(x$bbox)
  }
  
  yaml::write_yaml(x, file = filename)
  
  if ("bbox" %in% names(x)) x <- y
  
  x
}
