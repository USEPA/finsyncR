#' Access to invertebrate Trait data from the US
#'
#' @param database Which trait database should be retrieved?
#' Either \code{"Conus"}, \code{"Vieira"} or \code{"Both"}.
#' See \code{Details} below for more information.
#' @param format Should trait groups be displayed in
#' long - or wideformat, or should traits be transformed to
#' binary coding ? Either \code{"long"}, \code{"wide"},
#' or \code{"binary"}. \code{"long"} and \code{"wide"} only work for
#' the Conus dataset. See \code{Details} below for more information.
#' @param meta logical. Should metainformation (e.g. definition of traits)
#' be displayed or removed?
#' \code{TRUE} or \code{FALSE}. See \code{Details}
#' below for more information.
#'
#'
#' @return A taxa by trait data frame with taxa and trait information.
#'
#' @author Stefan Kunz, Michael Mahon, Samantha Rumschlag
#'
#' @details
#' \code{database} Returns either the Conus trait database retrieved from
#' https://portal.edirepository.org/nis/mapbrowse?packageid=edi.481.5
#' or the Vieiera et al., 2006 trait database
#' (related publication: https://pubs.usgs.gov/ds/ds187/pdf/ds187.pdf), or both.
#' Currently, if both databases are specified they can only be returned in the
#' following formats: both binary, both wideformat, Vieira wide with categories &
#' Conus in longformat
#'
#' \code{format} For the Conus trait dataset:
#' Trait groups can be displayed in long format like:
#' 'Genus', 'Trait Group', 'Trait', 'Family', 'Order'.
#' Or in wide format, where each Trait group
#' is a separate column with individual traits displayed as categories.
#' For the Vieira dataset traits are displayed in wideformat with trait groups
#' as columns and traits as categories, or if \code{"binary"} is specified,
#' catgeorical traits are transformed to binary coding. Setting \code{format}
#' to \code{"wide"} and \code{database} to \code{"Vieira"} does not affect
#' the data processing of the \code{"Vieira"} trait database.
#' If \code{"binary"} is specified, column names
#' are named like: 'TraitGroup_individualtrait' (e.g. Voltinism_abbrev_Semivoltine)
#'
#' \code{meta} Currently returns trait definitions for the
#'  Conus trait database.
#' If \code{meta} is set \code{TRUE} than the Conus trait database
#' and the trait definitions are returned as a list object.
#' @export

getInvertTraitData <- function(database = "Conus",
                               format = "wide",
                               meta = TRUE) {
    if (!database %in% c("Conus", "Vieira", "Both")) {
        stop("database must be either set to 'Conus', 'Viera', or 'Both'.")
    }

    if (!format %in% c("long", "wide", "binary")) {
        stop("format must be set to either 'long' or 'wide'.")
    }

    if (!isFALSE(meta) && !isTRUE(meta)) {
        stop("meta must be set to either TRUE or FALSE")
    }

    # Vieira Database
    # Preprocessed version (mainly retrieved and corrected
    # taxonomic information) in inst/extdata
    if (database == "Vieira" || database == "Both") {
        TraitDB_Vieira <- readRDS(
            file = base::system.file("extdata",
                                     "Traits_Vieira_pp.rds",
                                     package = "StreamData")
        )

        # remove a few columns that contain additional information
        # TODO: Could be put into an additionald data.frame and be returned
        # as metainformation
        # (includes also information on water body type)
        rm_col <-
            grep(
                "(?i)Study.*|comment.*|TraitRecord.*|^Adult$|Data.*|WB.*|Morph.*|Dev.*",
                names(TraitDB_Vieira),
                value = TRUE
            )
        TraitDB_Vieira[, (rm_col) := NULL]

        # unique_id needed for duplicates
        TraitDB_Vieira[, unique_id := paste0("id_", 1:nrow(TraitDB_Vieira))]

        # categorical traits converted into binary
        if (database != "Both") {
            if (format == "binary") {
                char_cols <- names(Filter(is.character, TraitDB_Vieira))
                TraitDB_Vieira_lf <- data.table::melt(
                    data = TraitDB_Vieira[, .SD, .SDcols = char_cols],
                    id.vars = c(
                        "Species",
                        "Genus",
                        "Family",
                        "Order",
                        "Taxon",
                        "unique_id"
                    ),
                    value.name = "Trait",
                    variable.name = "Trait_group"
                )

                # combine trait name & category names
                TraitDB_Vieira_lf[!is.na(Trait),
                                  Trait := paste0(Trait_group, "_", Trait)]

                # convert back to long format
                TraitDB_Vieira_bin <- data.table::dcast(
                    data = TraitDB_Vieira_lf[!is.na(Trait), ],
                    formula = unique_id + Species + Genus + Family + Order ~
                        Trait,
                    fun.aggregate = length
                )

                rm_cols <-
                    intersect(names(TraitDB_Vieira), char_cols)
                rm_cols <- rm_cols[rm_cols != "unique_id"]

                # merge back non-categorical traits
                TraitDB_Vieira[, (rm_cols) := NULL]
                TraitDB_Vieira <-
                    data.table::merge.data.table(x = TraitDB_Vieira_bin,
                                                 y = TraitDB_Vieira,
                                                 by = "unique_id")
            }
            return(TraitDB_Vieira)
        }
    }

    # Download GenusTraits_Conus.csv from
    # https://portal.edirepository.org/nis/mapbrowse?packageid=edi.481.
    # Could improve this by reading in the html page and searching
    # for the "Freshwater insects CONUS: Insect Traits by
    # Genus" header and getting the link from there
    # (only necessary if the link will change in the future)
    if (database == "Conus" || database == "Both") {
        GenusTraits_Conus <-
            data.table::fread(
                "https://portal.edirepository.org/nis/dataviewer?packageid=edi.481.5&entityid=3a88bfdfefcfe6dcafb27afd3ce4e90c"
            )

        # Load TaxonomyConus from Conus DB
        TaxonomyConus <-
            data.table::fread(
                "https://portal.edirepository.org/nis/dataviewer?packageid=edi.481.5&entityid=64a5bd1621948af6f486b105f591791f"
            )

        # Load metainformation (trait definitions)
        if (isTRUE(meta)) {
            TraitDefinition_Conus <-
                data.table::fread(
                    "https://portal.edirepository.org/nis/dataviewer?packageid=edi.481.5&entityid=0268356db22e387410820f0260340aa9"
                )
        }

        # Merge taxonomic information
        GenusTraits_Conus[TaxonomyConus,
                          `:=`(Family = i.Family,
                               Order = i.Order),
                          on = "Genus"]

        # Transformation of Trait Groups and individual traits
        # TODO Return of the metainformation can probably
        # be implemented in a smarter way
        # (DRY principle)
        if (format == "long") {
            if (database == "Both") {
                if (isTRUE(meta)) {
                    return(
                        list(
                            "Conus_longformat" = GenusTraits_Conus,
                            "Trait_definitions_Conus" = TraitDefinition_Conus,
                            "Vieira_TDB" = TraitDB_Vieira
                        )
                    )
                }
                return(
                    list(
                        "Conus_longformat" = GenusTraits_Conus,
                        "Vieira_TDB" = TraitDB_Vieira
                    )
                )
            }
            if (isTRUE(meta) && database != "Both") {
                return(
                    list(
                        "Conus_longformat" = GenusTraits_Conus,
                        "Trait_definitions_Conus" = TraitDefinition_Conus
                    )
                )
            }
            return(GenusTraits_Conus)
        }
        if (format == "wide") {
            GenusTraits_Conus_wide <- data.table::dcast(GenusTraits_Conus,
                                                        Genus + Family + Order ~ Trait_group,
                                                        value.var = "Trait")
            if (database == "Both") {
                if (isTRUE(meta)) {
                    return(
                        list(
                            "Conus_wideformat" = GenusTraits_Conus_wide,
                            "Trait_definitions_Conus" = TraitDefinition_Conus,
                            "Vieira_TDB" = TraitDB_Vieira
                        )
                    )
                }
                return(
                    list(
                        "Conus_wideformat" = GenusTraits_Conus_wide,
                        "Vieira_TDB" = TraitDB_Vieira
                    )
                )
            }
            if (isTRUE(meta) && database != "Both") {
                return(
                    list(
                        "Conus_wideformat" = GenusTraits_Conus_wide,
                        "Trait_definitions_Conus" = TraitDefinition_Conus
                    )
                )
            }
            return(GenusTraits_Conus_wide)
        }
        if (format == "binary") {
            GenusTraits_Conus <- GenusTraits_Conus[!is.na(Trait), ]
            GenusTraits_Conus[, Trait := paste0(Trait_group, "_", Trait)]
            GenusTraits_Conus_binary <-
                data.table::dcast(GenusTraits_Conus,
                                  Genus + Family + Order ~ Trait,
                                  fun.aggregate = length)
            if (database == "Both") {
                if (isTRUE(meta)) {
                    return(
                        list(
                            "Conus_traits_binary_coded" = GenusTraits_Conus_binary,
                            "Trait_definitions_Conus" = TraitDefinition_Conus,
                            "Vieira_TDB" = TraitDB_Vieira
                        )
                    )
                }
                return(
                    list(
                        "Conus_traits_binary_coded" = GenusTraits_Conus_binary,
                        "Vieira_TDB" = TraitDB_Vieira
                    )
                )
            }
            if (isTRUE(meta) && database != "Both") {
                return(
                    list(
                        "Conus_traits_binary_coded" = GenusTraits_Conus_binary,
                        "Trait_definitions_Conus" = TraitDefinition_Conus
                    )
                )
            }
            return(GenusTraits_Conus_binary)
        }
    }
}
