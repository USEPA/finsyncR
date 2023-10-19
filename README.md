
<!-- README.md is generated from README.Rmd. Please edit that file -->

# finsyncR -this is where the hexsticker will go

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/StreamData/finsyncR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/StreamData/finsyncR/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of finsyncR is to provide easy access and automated data
management to USGS and EPA stream BioData, pesticide county use
estimates, and measured water quality data.

## Installation

Note: because this package is held within a private repository on
GitHub, you will need to use the `auth_token` argument within
`install_github()`. Generate a personal access token (PAT) in
<https://github.com/settings/tokens> and supply the provided code to
this argument.

You can install the released version of `finsyncR` from
[GitHub](https://github.com/StreamData/finsyncR) with:

    library(devtools)

    devtools::install_github("StreamData/finsyncR",
                             auth_token = "tokenstring",
                             build_vignette = TRUE
                             )

## Example

This is a basic example which shows you how to use one of the
*getBioData* set of functions to access the fish dataset.

``` r
library(finsyncR)

Fish <- getFishData(taxonLevel = "Species")
#> Joining with `by = join_by(SITE_ID, YEAR, VISIT_NO)`
#> Joining with `by = join_by(UID, SITE_ID, VISIT_NO)`
#> Joining with `by = join_by(SITE_ID, DATE_COL, VISIT_NO)`

head(Fish)[,c(1:5,36,90,400)]
#>   Agency ProjectLabel    SiteNumber      StudyReachName CollectionDate
#> 1   USGS     MMSD Eco USGS-04086600 04086600-A-MMSD Eco     2007-09-10
#> 2   USGS     MMSD Eco USGS-04087030 04087030-A-MMSD Eco     2007-09-05
#> 3   USGS     MMSD Eco USGS-04087070 04087070-A-MMSD Eco     2007-09-05
#> 4   USGS     MMSD Eco USGS-04087088 04087088-A-MMSD Eco     2007-09-04
#> 5   USGS     MMSD Eco USGS-04087119 04087119-A-MMSD Eco     2007-09-12
#> 6   USGS     MMSD Eco USGS-04087204 04087204-A-MMSD Eco     2007-09-07
#>   Cyprinus.carpio Cyprinella.galactura Oreochromis.aureus
#> 1               0                    0                  0
#> 2               0                    0                  0
#> 3               0                    0                  0
#> 4               0                    0                  0
#> 5               0                    0                  0
#> 6               0                    0                  0
```

## Metadata

Metadata can be found in the “Metadata” vignette:
`vignette("Metadata", package = "finsyncR")`.

## Using finsyncR

A compendium on the correct use of the finsyncR package and its output
datasets can be found in the “Getting Started” vignette:
`vignette("GettingStarted", package = "finsyncR")`.

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
