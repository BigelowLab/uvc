% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/version.R
\name{build_version_path}
\alias{build_version_path}
\title{Build a version output structure, or if existing then check it}
\usage{
build_version_path(cfg, subdirs = c("model", "xcast", "image"))
}
\arguments{
\item{cfg}{configuration list}

\item{subdirs}{character vector of subdirectories to create}
}
\value{
the root path for the version
}
\description{
Build a version output structure, or if existing then check it
}
