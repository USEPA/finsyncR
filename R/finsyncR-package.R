#' finsyncR: An R Package to Access Cleaned and Compiled USGS and EPA Stream Data
#'
#'@description
#' The `finsyncR` package provides the data and functions to generate
#'   site by taxa datasets for macroinvertebrates and fish samples from the EPA
#'   National Rivers and Streams Assessment and USGS Biodata repository.
#'
#' Users should cite the aforementioned datasets as follows:
#'
#' * U.S. Geological Survey, 2020. BioData - Aquatic bioassessment data for the
#'   Nation: U.S. Geological Survey database, accessed 17 December 2020,
#'   at https://doi.org/10.5066/F77W698B
#'
#' * U.S. Environmental Protection Agency. 2006. National Aquatic Resource
#'   Surveys. Wadeable Streams Assessment 2004 (data and metadata files).
#'   Available from U.S. EPA web page:
#'   https://www.epa.gov/national-aquatic-resource-surveys/data-national-aquatic-resource-surveys.
#'
#' * U.S. Environmental Protection Agency. 2016. National Aquatic Resource
#'   Surveys. National Rivers and Streams Assessment 2008-2009 (data and metadata
#'   files). Available from U.S. EPA web page:
#'   https://www.epa.gov/national-aquatic-resource-surveys/data-national-aquatic-resource-surveys.
#'
#' * U.S. Environmental Protection Agency. 2020. National Aquatic Resource
#'   Surveys. National Rivers and Streams Assessment 2013-2014 (data and metadata
#'   files). Available from U.S. EPA web page:
#'   https://www.epa.gov/national-aquatic-resource-surveys/data-national-aquatic-resource-surveys.
#'
#' * U.S. Environmental Protection Agency. 2022. National Aquatic Resource
#'   Surveys. National Rivers and Streams Assessment 2018-2019 (data and metadata
#'   files). Available from U.S. EPA web page:
#'   https://www.epa.gov/national-aquatic-resource-surveys/data-national-aquatic-resource-surveys.
#'
#' Metadata can be found in the "Metadata"
#' vignette: [vignette("Metadata", package = "finsyncR")].
#'
#' A compendium on the correct use of the finsyncR package and its output
#' datasets can be found in the "Getting Started"
#' vignette: [vignette("GettingStarted", package = "finsyncR")].
#'
#' **Of note**: The approach to data management used in this package is not the
#' same as the approach used in the EPA NARS national reports. For more
#' information on the data management approach used in the national
#' assessments please see:
#'
#'
#' **Disclaimer**: The United States Environmental Protection Agency (EPA) GitHub
#' project code is provided on an "as is" basis and the user assumes
#' responsibility for its use. EPA has relinquished control of the information
#' and no longer has responsibility to protect the integrity , confidentiality,
#' or availability of the information. Any reference to specific commercial
#' products, processes, or services by service mark, trademark, manufacturer,
#' or otherwise, does not constitute or imply their endorsement, recommendation
#' or favoring by EPA. The EPA seal and logo shall not be used in any manner to
#' imply endorsement of any commercial product or activity by EPA or the United
#' States Government.
#'
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import tidyselect
#' @import lubridate
#' @import httr
#' @rawNamespace import(data.table, except = c(last, first, between, month, hour, quarter, week, year, wday, second, minute, mday, yday, isoweek))
#' @importFrom utils read.csv
#' @importFrom utils read.table
#' @keywords internal
"_PACKAGE"
#' @docType package
#' @aliases finsyncR-package, finsyncR?finsyncR, finsyncR
#'
#' @seealso `browseVignettes("finsyncR")`.
#'
#' @name finsyncR

## usethis namespace: start
## usethis namespace: end
NULL
