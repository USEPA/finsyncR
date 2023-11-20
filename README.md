
<!-- README.md is generated from README.Rmd. Please edit that file -->

# finsyncR -this is where the hexsticker will go

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![R-CMD-check](https://github.com/USEPA/finsyncR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/USEPA/finsyncR/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`finsyncR` (**f**ish and **in**vertebrate **sync**hronizer in **R**) is
a data management package that integrates and processes national-level
aquatic biomonitoring datasets in the U.S., with a focus on fish and
macroinvertebrates sampled in rivers and streams. The package
streamlines the process of retrieving and harmonizing these data,
improving access to cleaned data and making the application these data
straightforward for researchers. The sources of data for this package
are the United States Environmental Protection Agency’s (USEPA) National
Aquatic Resource Surveys (NARS), namely the National River and Streams
Assessment (NRSA), and United States Geological Survey’s (USGS) BioData.

## Installation

You can install the released version of `finsyncR` from
[GitHub](https://github.com/USEPA/finsyncR) with:

    library(devtools)

    devtools::install_github("USEPA/finsyncR",
                             build_vignette = TRUE)

## Example of function and output

This is a basic example which shows you how to use the `getFishData()`
function to generate the fish dataset for both USGS and USEPA datasets.

``` r
library(finsyncR)

Fish <- getFishData(taxonLevel = "Species",
                    agency = c("USGS","EPA"))
#>  Gathering and cleaning USGS raw data                     Applying taxonomic fixes to USGS data                     Applying count standardization to USGS data                     Gathering, joining, and cleaning EPA raw data                     Applying taxonomic fixes to EPA data                     Applying count standardization to EPA data                     Harmonizing USGS and EPA data                                      Finalizing data for output                           finsyncR data synchronization complete

head(Fish)[,c(1:5,36,90,400)]
#>   Agency ProjectLabel    SiteNumber      StudyReachName CollectionDate
#> 1   USGS     MMSD Eco USGS-04086600 04086600-A-MMSD Eco     2007-09-10
#> 2   USGS     MMSD Eco USGS-04087030 04087030-A-MMSD Eco     2007-09-05
#> 3   USGS     MMSD Eco USGS-04087070 04087070-A-MMSD Eco     2007-09-05
#> 4   USGS     MMSD Eco USGS-04087088 04087088-A-MMSD Eco     2007-09-04
#> 5   USGS     MMSD Eco USGS-04087119 04087119-A-MMSD Eco     2007-09-12
#> 6   USGS     MMSD Eco USGS-04087204 04087204-A-MMSD Eco     2007-09-07
#>   Ameiurus.melas Clinostomus.funduloides Strongylura.marina
#> 1              0                       0                  0
#> 2              0                       0                  0
#> 3              0                       0                  0
#> 4              0                       0                  0
#> 5              0                       0                  0
#> 6              0                       0                  0
```

## Metadata

Metadata can be found in the “Metadata” vignette:
`vignette("Metadata", package = "finsyncR")`.

## Using finsyncR

A compendium on the correct use of the finsyncR package and its output
datasets can be found in the “Getting Started” vignette:
`vignette("GettingStarted", package = "finsyncR")`.

## Contributing to finsyncR

For information on how to contribute to the `finsyncR` package, please
see the [contributing to finsyncR
document](https://github.com/USEPA/finsyncR/blob/main/CONTRIBUTING.md).

## Open-Source Code Policy

Effective August 8, 2016, the [OMB Mandate: M-16-21; Federal Source Code
Policy: Achieving Efficiency, Transparency, and Innovation through
Reusable and Open Source
Software](https://obamawhitehouse.archives.gov/sites/default/files/omb/memoranda/2016/m_16_21.pdf)
applies to new custom-developed code created or procured by EPA
consistent with the scope and applicability requirements of Office of
Management and Budget’s (OMB’s) Federal Source Code Policy. In general,
it states that all new custom-developed code by Federal Agencies should
be made available and reusable as open-source code.

The EPA specific implementation of OMB Mandate M-16-21 is addressed in
the [System Life Cycle Management
Procedure](https://www.epa.gov/irmpoli8/policy-procedures-and-guidance-system-life-cycle-management-slcm).
EPA has chosen to use GitHub as its version control system as well as
its inventory of open-source code projects. EPA uses GitHub to inventory
its custom-developed, open-source code and generate the necessary
metadata file that is then posted to code.gov for broad reuse in
compliance with OMB Mandate M-16-21.

If you have any questions or want to read more, check out the [EPA Open
Source Project Repo](https://github.com/USEPA/open-source-projects) and
[EPA’s Interim Open Source Code
Guidance](https://www.epa.gov/developers/open-source-software-and-epa-code-repository-requirements).

## License

All contributions to this project will be released under the CCO-1.0
license file dedication. By submitting a pull request or issue, you are
agreeing to comply with this waiver of copyright interest.

## Disclaimer

The United States Environmental Protection Agency (EPA) GitHub project
code is provided on an “as is” basis and the user assumes responsibility
for its use. EPA has relinquished control of the information and no
longer has responsibility to protect the integrity , confidentiality, or
availability of the information. Any reference to specific commercial
products, processes, or services by service mark, trademark,
manufacturer, or otherwise, does not constitute or imply their
endorsement, recommendation or favoring by EPA. The EPA seal and logo
shall not be used in any manner to imply endorsement of any commercial
product or activity by EPA or the United States Government.
