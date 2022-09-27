UVC
================

## Ungulate Vehicle Collisions

This package provides workflow support for modeling and predicting
ungulate vehicle collisions. Note that what we model are reported
animal-vehicle interactions that result in police/transportation
records.

## Requirements

-   [R v4.1+](https://www.r-project.org/)

-   [rlang](https://CRAN.R-project.org/package=rlang)

-   [dplyr](https://CRAN.R-project.org/package=dplyr)

-   [readr](https://CRAN.R-project.org/package=readr)

-   [yaml](https://CRAN.R-project.org/package=yaml)

-   [sf](https://CRAN.R-project.org/package=sf)

-   [stars](https://CRAN.R-project.org/package=stars)

-   [suncalc](https://CRAN.R-project.org/package=suncalc)

## Installation

    remotes::install_github("BigelowLab/uvc")

## Utilities

#### Configuration

We build the workflow around a configuration, which we store in YAML
format, and we typically assign to variable `CFG`. This is human
readable and simply becomes a convenient named list in R. See more more
about configuration in the
[wiki](https://github.com/BigelowLab/uvc/wiki/Configurations). To
simplify workflow steps, the configuration is often the first (and
possibly the only) argument for many of the functions exposed by this
package.

Note that we provide some utility to implement limited `autopopulation`
for paths. For example, when we read the YAML if we encounter an element
named `datapath` then we scan the rest of the configuration for
`$datapath` (note the leading `$`). We uses these to flag where
substitution is required… one less thing for users to edit.

    CFG <- uvc::read_configuration(`/path/to/config.yaml`)

#### Occurrences (observations of events)

Occurrences of UVCs are stored in a tabular format (likely CSV); these
must have location and time information. Other information may be
included. We typically assign this to variable `CRASHES`. The occurrence
records are created from public incident reports (police reports,
transportation records, etc.) managed as spatial point data within R.
One variable we add is a pre-computed cell number that matches a valid
cells in rasterized covariates. This is not a requirement, but it has
proven efficient for our purposes.

Here we see how we might use the configuration, `CFG`.

    CRASHES <- uvc::read_occurrence(CFG, ...)

#### Covariates (predicitors)

##### Static covariates

Static covariates are organized as spatial point data within R which we
typically assign to variable `ROADS`. These are sampled along the road
network at predefined locations. In our earliest work, this collation
was performed outside of R. Covariates include metrics about the road
itself (classification, orientation, slope, etc) as well as metrics
about the surrounding terrain (water access, land cover, etc).

The density of the sample set is determined by the user and covariates
are collated by the user prior to use of this package.

Jut as for occurrence points, pre-computed raster cell numbers will
speed subsequent extraction of rasterized dynamic variable.

**NOTE** Occurence records (`CRASHES`) may or may not exactly coincide
with static `ROADS`. We make the assumption that the closest `ROADS`
point is a suitable representation for each `CRASHES` point. See the
`relocate_occurrences()` function for details.

    ROADS <- uvc::read_covariate_points(CFG, ...)

##### Dynamic covariates

Dynamic covariates may include weather, ambient light and other
time-dependent variables. To date we have used daily averages of [NAM
218 weather
models](https://www.ncei.noaa.gov/products/weather-climate-models/north-american-mesoscale),
and computation of the orientation of the sun by hour. In each case, to
create model input datasets we create spatial point datasets.

It is here that `cell` number can speed computation - weather data is
rasterized, pre-computing occurrence locations can make extraction
quick.

**NOTE** that the tools exposed by the package try to be agnostic about
the sources of these datasets so that we may substitute or modify the
inputs in the future. To provide this flexibility most of the
implementation is not withing the package *per se*, but instead are
implemented in the ancillary
[scripts](https://github.com/BigelowLab/uvc/tree/main/inst/scripts). Use
these scripts as a template for subsequent modifications.

## Scripts

While this package provides **utility** functions for managing the
modeling workflow, it also provides template
[scripts](https://github.com/BigelowLab/uvc/tree/main/inst/scripts) for
modeling and predicting.

[v0.000-model-doy.R](https://github.com/BigelowLab/uvc/tree/main/inst/scripts/v0.000-model-doy.R)
provides an example that computes a maxent model for a day of year (or a
range of days). It does so my selecting data within a user specified
window of time. Occurrences and and covariartes are collated to be fed
into the
[maxent](https://biodiversityinformatics.amnh.org/open_source/maxent/)
presence-only model.

Note that the second day-of-year is optional. If present than models are
created for each step in the sequence `doy1, doy1+1, doy1+2, ..., doy2`

    $ Rscript /path/to/script/v0.000-model-doy.R /path/to/config.yaml doy1 [doy2]

[v0.000-xcast-date.R](https://github.com/BigelowLab/uvc/tree/main/inst/scripts/v0.000-xcast-date.R)
accepts a date upon which to make a prediction by using the model for
the matching day. On that day it creates 24 prediction (one each for
hours 00 - 24 or some other increment specifed in the configuration.)

Note that the second date is optional. If present than models are
created for each day in the sequence
`date1, date1+1, date1+2, ..., date2`

    $ Rscript /path/to/script/v0.000-xcast-date.R /path/to/config.yaml date1 [date2]
