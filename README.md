
<!-- README.md is generated from README.Rmd. Please edit that file -->

# StreamData

<!-- badges: start -->

<!-- badges: end -->

The goal of StreamData is to provide easy access and automated data
management to USGS and EPA stream data, pesticide county use estimates, and
measured water quality data.

## Installation

Note: because this package is held within a private repository on
GitHub, you will need to use the `auth_token` argument within
`install_github()`. Generate a personal access token (PAT) in
<https://github.com/settings/tokens> and supply the provided code to
this argument.

You can install the released version of `StreamData` from
[GitHub](https://github.com/StreamData/StreamData) with:

    library(devtools)
    
    devtools::install_github("StreamData/StreamData",
                             auth_token = "tokenstring",
                             build_vignette = FALSE
                             )

## Example

This is a basic example which shows you how to use one of the
*getBioData* set of functions to access the fish dataset.

``` r
library(StreamData)

Fish <- getFishData(taxonLevel = "Family")

head(Fish)[,1:35]
#>   ProjectLabel ProjectAssignedSampleLabel     NAWQA.SMCOD NAWQAStudyUnitCode
#> 1  DELR BioTDB                            DELR0799FSH0002               DELR
#> 2  DELR BioTDB                            DELR0799FSH0005               DELR
#> 3  COOK BioTDB                            COOK0899FSH0002               COOK
#> 4  COOK BioTDB                            COOK0899FSH0001               COOK
#> 5  COOK BioTDB                            COOK0999FSH0004               COOK
#> 6  COOK BioTDB                            COOK1099FSH0006               COOK
#>   CollectionDate StartTime TimeDatum CollectionYear CollectionMonth
#> 1     1999-07-15     10:22                     1999               7
#> 2     1999-07-21     09:00                     1999               7
#> 3     1999-08-10     13:25                     1999               8
#> 4     1999-08-09     15:05                     1999               8
#> 5     1999-09-01     08:45                     1999               9
#> 6     1999-10-14     16:00                     1999              10
#>   CollectionDayOfYear ProvisionalData SiteNumber
#> 1                 196                   01477120
#> 2                 202                   01470779
#> 3                 222                   15274000
#> 4                 221                   15275100
#> 5                 244                   15241600
#> 6                 287                   15294100
#>                                        SiteName          StudyReachName
#> 1              Raccoon Creek near Swedesboro NJ 01477120-A-NAWQA-BioTDB
#> 2          Tulpehocken Creek near Bernville, PA 01470779-A-NAWQA-BioTDB
#> 3                 SF CAMPBELL C NR ANCHORAGE AK 15274000-C-NAWQA-BioTDB
#> 4 CHESTER C AT ARCTIC BOULEVARD AT ANCHORAGE AK 15275100-C-NAWQA-BioTDB
#> 5                   NINILCHIK R AT NINILCHIK AK 15241600-C-NAWQA-BioTDB
#> 6                         DESHKA R NR WILLOW AK 15294100-C-NAWQA-BioTDB
#>   SiteTypeName StateFIPSCode CountyFIPSCode Latitude_dd Longitude_dd
#> 1       Stream            34             15    39.74056    -75.25917
#> 2       Stream            42             11    40.41343    -76.17161
#> 3       Stream             2             20    61.16667   -149.77275
#> 4       Stream             2             20    61.20473   -149.89748
#> 5       Stream             2            122    60.04842   -151.66492
#> 6       Stream             2            170    61.76752   -150.33918
#>   CoordinateDatum      HUCCode DrainageArea_mi2  MethodCode MethodBasic
#> 1           NAD83      2040202             26.9 Backpack-P1    Shocking
#> 2           NAD27      2040203             70.4    Towed-P1    Shocking
#> 3           NAD27 190204010601             26.8 Backpack-P1    Shocking
#> 4           NAD27 190204010806             29.5 Backpack-P1    Shocking
#> 5           NAD83 190203010503            135.0 Backpack-P1    Shocking
#> 6           NAD27 190205050910            597.0 Backpack-P1    Shocking
#>   ReachLengthFished_m NumberSeineHauls NumberStationarySetsKicks
#> 1                 157               NA                        NA
#> 2                 270               NA                        NA
#> 3                 150               NA                        NA
#> 4                 150               NA                        NA
#> 5                 150               NA                        NA
#> 6                 150               NA                        NA
#>   NumberSnorkelingTransects MinutesShockTime StandardMethod Anguillidae
#> 1                        NA       125.683333     125.683333   0.1266959
#> 2                        NA       104.133333     104.133333   0.0000000
#> 3                        NA         6.716667       6.716667   0.0000000
#> 4                        NA        13.500000      13.500000   0.0000000
#> 5                        NA        51.166667      51.166667   0.0000000
#> 6                        NA         6.666667       6.666667   0.0000000
#>   Aphredoderidae Catostomidae Centrarchidae   Clupeidae
#> 1    0.000810854    0.1589274    0.02625140 0.000810854
#> 2    0.000000000    2.9546166    0.04709062 0.000000000
#> 3    0.000000000    0.0000000    0.00000000 0.000000000
#> 4    0.000000000    0.0000000    0.00000000 0.000000000
#> 5    0.000000000    0.0000000    0.00000000 0.000000000
#> 6    0.000000000    0.0000000    0.00000000 0.000000000
```
