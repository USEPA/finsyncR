#' finsyncR: An R Package to synchronize fish and invertebrate datasets from federal sources in the United States
#'
#'@description
#' `finsyncR` (**f**ish and **in**vertebrate **sync**hronizer in **R**) flexibly
#' acquires, processes, and integrates national-level aquatic biomonitoring
#' datasets in the US, with a focus on fish and macroinvertebrates sampled in
#' rivers and streams. The package streamlines
#' the process of retrieving and working with these data, making the process of
#' utilizing these data  easier for researchers. The data resources used in this
#' package are from United States Environmental Protection Agency's National
#' Aquatic Resources Surveys, namely the National River and Streams Assessment,
#' and United States Geological Survey's BioData.
#'
#` The package provides abundance and occurrence data for fish and macroinvertebrates
#` at a variety of levels of taxonomic resolution. The package provides flexibility
#` for common data management issues for temporal biologic datasets. Data management
#` options include harmonization of macroinvertebrate taxonomy across the two federal
#` datasets and through time and the ability to drop hybrid organisms altogether for
#` fish. Samples, which often vary in the number of individuals within a sample,
#` can be rarefied to a specific count. Additionally, fish samples can be standardized
#` to catch per-unit effort and to account for catchability differences between
#` fish sampling methods. The package also matches sampling sites for fish and
#` macroinvertebrates to data from the National Land Cover Database. Please see
#` the Getting Started Vignette, for more information on how to use the package
#` and on certain nuances of the data that could influence analyses in some
#` instances including spatial sampling designs, the timing and frequency of
#` monitoring efforts, variation in sampling efforts, and improvements in taxonomic
#` identifications through time in some orders of macroinvertebrates.
#'
#' Data from USGS BioData were obtained from Pete Ruhl (pmruhl@usgs.gov), and
#' data from the National Aquatic Resource Surveys are directly downloaded by
#' the package from the [NARS website](https://www.epa.gov/national-aquatic-resource-surveys/data-national-aquatic-resource-surveys).
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
#' vignette: `vignette("Metadata", package = "finsyncR")`.
#'
#' A compendium on the correct use of the finsyncR package and its output
#' datasets can be found in the "Getting Started"
#' vignette: `vignette("GettingStarted", package = "finsyncR")`.
#'
#' In addition, users may wish to calculate abundances and raw counts or to
#' rarefy raw counts. This functionality is not inherently provided within
#' the `getInvertData()` function, but code to make these calculations can be
#' found in the "Calculating Abundances, Raw Counts, and Rarefying Raw Counts"
#' vignette `vignette("BackCalculation", package = "finsyncR")`.
#'
#' **Disclaimers**
#' The approaches provided in this package concerning the way the BioData and
#' NRSA data are treated are not necessarily the same as the approaches used in
#' other USGS and EPA publications or national assessments. So, replicating
#' analyses using the processed data in this package may not produce identical
#' results. For instance, multimetric indices produced from NRSA data from
#' `finsyncR` may not match multimetric indices in EPA national assessment reports.
#'
#' The methods developed to join the EPA and USGS datasets described in this
#' manuscript have not been designed with the intent of adding other fish and
#' macroinvertebrate datasets. So, the appropriateness of these methods cannot
#' be guaranteed for other datasets.
#'
#' Finally, the United States Environmental Protection Agency (EPA) GitHub
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
