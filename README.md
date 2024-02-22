
<!-- README.md is generated from README.Rmd. Please edit that file -->

# finsyncR <a href="https://usepa.github.io/finsyncR/"><img src="man/figures/logo.png" align="right" height="138" /></a>

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
Users should be aware that the `getFishData()` and `getInvertData()`
functions take a fair amount of time to run. While running, the
functions print statements updating the user on the progress of the
workflow.

``` r
library(finsyncR)

Fish <- getFishData(taxonLevel = "Species",
                    agency = c("USGS","EPA"))

head(Fish)[,c(1:5,36,90,400)]
#>   Agency      SampleID ProjectLabel    SiteNumber      StudyReachName
#> 1   USGS BDB-000025003     MMSD Eco USGS-04086600 04086600-A-MMSD Eco
#> 2   USGS BDB-000025011     MMSD Eco USGS-04087030 04087030-A-MMSD Eco
#> 3   USGS BDB-000025015     MMSD Eco USGS-04087070 04087070-A-MMSD Eco
#> 4   USGS BDB-000025019     MMSD Eco USGS-04087088 04087088-A-MMSD Eco
#> 5   USGS BDB-000025023     MMSD Eco USGS-04087119 04087119-A-MMSD Eco
#> 6   USGS BDB-000025034     MMSD Eco USGS-04087204 04087204-A-MMSD Eco
#>   Percina.caprodes Notropis.telescopus Notropis.amabilis
#> 1                0                   0                 0
#> 2                0                   0                 0
#> 3                0                   0                 0
#> 4                0                   0                 0
#> 5                0                   0                 0
#> 6                0                   0                 0
```

## Possible uses of finsyncR

Potential uses of the finsyncR package and resulting data include
studies of changes in fish and macroinvertebrate communities across
space and time and the environmental causes and ecological consequences
of those changes. The table below provides example code to generate
macroinvertebrates and fish datasets given hypothetical research
questions.

| Research question                                                                                                            | Output dataset requested                                                                                                                               |                                                                                                                                                                                                                                                                         Macroinvertebrates |                                                                                                                                                                                                                        Fish |
|:--------------------------------------------------------------------------------------------------------------------------|:----------------------------------------------------------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------:|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------:|
| How does density/CPUE change through time across EPA and USGS samples?                                                       | Density/CPUE at sites for lowest taxonomic resolution, with shared taxa only, removing hybrid fish, and excluding boatable streams/rivers              |                                  `getInvertData(dataType = "density",` <br> `taxonLevel = "Genus",` <br> `taxonFix = "lump",` <br> `agency = c("USGS","EPA"),` <br> `lifestage = FALSE,` <br> `rarefy = FALSE,` <br> `sharedTaxa = TRUE,` <br> `seed = 0,` <br> `boatableStreams = FALSE)` |          `getFishData(dataType = "density",` <br> `taxonLevel = "Species",` <br> `agency = c("USGS","EPA"),` <br> `standardize = "CPUE",` <br> `hybrids = FALSE,` <br> `sharedTaxa = TRUE,` <br> `boatableStreams = FALSE)` |
| How does the composition of families differ between EPA and USGS samples?                                                    | Occurrence at sites for Family resolution, with all taxa, rarefying macroinvertebrate counts to 300 individuals, and excluding boatable streams/rivers |          `getInvertData(dataType = "occur",` <br> `taxonLevel = "Family",` <br> `taxonFix = "none",` <br> `agency = c("USGS","EPA"),` <br> `lifestage = FALSE,` <br> `rarefy = TRUE,`<br> `rarefyCount = 300,` <br> `sharedTaxa = FALSE,` <br> `seed = 1,` <br> `boatableStreams = FALSE)` |                                    `getFishData(dataType = "occur",` <br> `taxonLevel = "Family",` <br> `agency = c("USGS","EPA"),` <br> `standardize = "none",` <br> `sharedTaxa = FALSE,` <br> `boatableStreams = FALSE)` |
| What are spatial occupancy patterns within the Upper-Midwest Ecoregion using the randomized sampling design of the EPA NARS? | Occurrence for lowest taxonomic resolution for only EPA samples, then filtered to the UWM Ecoregion                                                    | `getInvertData(dataType = "occur",` <br> `taxonLevel = "Family",` <br> `taxonFix = "none",` <br> `agency = c("EPA"),` <br> `rarefy = TRUE,`<br> `rarefyCount = 500,` <br> `sharedTaxa = FALSE,` <br> `seed = 1,` <br> `boatableStreams = TRUE) %>%` <br> `filter(NARS_Ecoregion == "UMW")` | `getFishData(dataType = "occur",` <br> `taxonLevel = "Family",` <br> `agency = c("EPA"),` <br> `standardize = "none",` <br> `sharedTaxa = FALSE,` <br> `boatableStreams = TRUE) %>%` <br> `filter(NARS_Ecoregion == "UMW")` |

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

## Packages of interest

Other R packages of potential interest to `finsyncR` users include
[StreamCatTools](https://github.com/USEPA/StreamCatTools/) and
[TADA](https://github.com/USEPA/tada). `StreamCatTools` is an R package
for accessing StreamCat data via the StreamCat API and for working with
site data in conjunction with StreamCat and NHDPlus. `TADA` is an R
package can be used to compile and evaluate Water Quality Portal (WQP)
data for samples collected from surface water monitoring sites on
streams and lakes.

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

The approaches provided in this package concerning the way the BioData
and NRSA data are treated are not necessarily the same as the approaches
used in other USGS and EPA publications or national assessments. So,
replicating analyses using the processed data in this package may not
produce identical results. For instance, multimetric indices produced
from NRSA data from `finsyncR` may not match multimetric indices in EPA
national assessment reports.

The methods developed to join the EPA and USGS datasets described in
this manuscript have not been designed with the intent of adding other
fish and macroinvertebrate datasets. So, the appropriateness of these
methods cannot be guaranteed for other datasets.

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
