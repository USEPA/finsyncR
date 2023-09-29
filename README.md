
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
                             build_vignette = FALSE
                             )

## Example

This is a basic example which shows you how to use one of the
*getBioData* set of functions to access the fish dataset.

``` r
library(finsyncR)

Fish <- getFishData(taxonLevel = "Species")

head(Fish)[,1:35]
#>   ProjectLabel ProjectAssignedSampleLabel NAWQA.SMCOD NAWQAStudyUnitCode
#> 1     MMSD Eco                                                          
#> 2     MMSD Eco                                                          
#> 3     MMSD Eco                                                          
#> 4     MMSD Eco           WMIC0907FSH0004A                               
#> 5     MMSD Eco                                                          
#> 6     MMSD Eco                                                          
#>   CollectionDate StartTime                   TimeDatum CollectionYear
#> 1     2007-09-10     10:20 CDT - Central Daylight Time           2007
#> 2     2007-09-05     10:52 CDT - Central Daylight Time           2007
#> 3     2007-09-05     07:40 CDT - Central Daylight Time           2007
#> 4     2007-09-04     14:10 CDT - Central Daylight Time           2007
#> 5     2007-09-12     13:00 CDT - Central Daylight Time           2007
#> 6     2007-09-07     08:55 CDT - Central Daylight Time           2007
#>   CollectionMonth CollectionDayOfYear SiteVisitSampleNumber    SiteNumber
#> 1               9                 253                     1 USGS-04086600
#> 2               9                 248                     1 USGS-04087030
#> 3               9                 248                     1 USGS-04087070
#> 4               9                 247                     4 USGS-04087088
#> 5               9                 255                     1 USGS-04087119
#> 6               9                 250                     1 USGS-04087204
#>   Agency                                SiteName      StudyReachName
#> 1   USGS      MILWAUKEE RIVER NEAR CEDARBURG, WI 04086600-A-MMSD Eco
#> 2   USGS  MENOMONEE RIVER AT MENOMONEE FALLS, WI 04087030-A-MMSD Eco
#> 3   USGS LITTLE MENOMONEE RIVER AT MILWAUKEE, WI 04087070-A-MMSD Eco
#> 4   USGS        UNDERWOOD CREEK AT WAUWATOSA, WI 04087088-A-MMSD Eco
#> 5   USGS            HONEY CREEK AT WAUWATOSA, WI 04087119-A-MMSD Eco
#> 6   USGS        OAK CREEK AT SOUTH MILWAUKEE, WI 04087204-A-MMSD Eco
#>   SiteTypeName Latitude_dd Longitude_dd CoordinateDatum     HUCCode
#> 1       Stream    43.28028    -87.94250           NAD83 40400030604
#> 2       Stream    43.17278    -88.10389           NAD83 40400030401
#> 3       Stream    43.12361    -88.04361           NAD83 40400030402
#> 4       Stream    43.05472    -88.04611           NAD83 40400030404
#> 5       Stream    43.04383    -88.00511           NAD83 40400030405
#> 6       Stream    42.92500    -87.87000           NAD83 40400020102
#>   DrainageArea_mi2 MethodBasic ReachLengthFished_m StandardMethod
#> 1            607.0       Seine                 300              3
#> 2             34.7       Seine                 150              3
#> 3             19.7       Seine                 150              3
#> 4             18.1       Seine                 150              5
#> 5             10.3       Seine                 150              3
#> 6             25.0       Seine                 150              3
#>    FISH_PROTOCOL Methods SAMPLED_FISH    COMID StreamOrder Luxilus.cornutus
#> 1 Large Wadeable   Seine         <NA> 19645438           1                1
#> 2 Small Wadeable   Seine         <NA> 19645438           1                1
#> 3 Small Wadeable   Seine         <NA> 19645438           1                1
#> 4 Small Wadeable   Seine         <NA> 19645438           1                1
#> 5 Small Wadeable   Seine         <NA> 19645438           1                0
#> 6 Small Wadeable   Seine         <NA> 19645438           1                0
#>   Micropterus.dolomieu Notropis.atherinoides Ambloplites.rupestris
#> 1                    1                     1                     0
#> 2                    0                     0                     0
#> 3                    0                     0                     0
#> 4                    0                     0                     0
#> 5                    0                     0                     0
#> 6                    0                     0                     0
#>   Ameiurus.natalis Campostoma.anomalum
#> 1                0                   0
#> 2                0                   0
#> 3                0                   0
#> 4                0                   1
#> 5                0                   0
#> 6                0                   0
```

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
