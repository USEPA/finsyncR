\name{getInvertTraitData}
\alias{getInvertTraitData}
\title{Access to North American Invertebrate trait data}
\usage{
getInvertTraitData(
  database = "Conus",
  formatTraitGroups = "wide",
  binary = TRUE,
  meta = TRUE
)
}
\arguments{
\item{database}{Which trait database should be retrieved? 
Either \code{"Conus"}, \code{"Vieira"} or \code{"Both"}.
See \code{Details} below for more information.}

\item{format}{
Should trait groups be displayed in
long - or wideformat, or should traits be transformed to 
binary coding ? Either \code{"long"}, \code{"wide"}, 
or \code{"binary"}. \code{"long"} and \code{"wide"} only work for
the Conus dataset. See \code{Details} below for more information.
}

\item{meta}{logical. Should metainformation (e.g. definition of traits) 
be displayed or removed? \code{TRUE} or \code{FALSE}. See \code{Details}
below for more information.}
}
\value{
A data table with macroinvertebrate traits and taxonomic information. 
When metainformation should be retrieved trait data and metainformation are 
returned as data tables in a list.
}
\description{
Access to North American Invertebrate trait data
}
\details{
    \code{database} Returns either the Conus trait database retrieved from 
    https://portal.edirepository.org/nis/mapbrowse?packageid=edi.481.5
    or the Vieiera et al., 2006 trait database 
    (related publication: https://pubs.usgs.gov/ds/ds187/pdf/ds187.pdf), or both.
    Currently, if both databases are specified they can only be returned in the 
    following formats: both binary, both wideformat, Vieira wide with categories &
    Conus in longformat.  

    \code{format} \code{format} For the Conus trait dataset: 
    Trait groups can be displayed in long format like: 
    'Genus', 'Trait Group', 'Trait', 'Family', 'Order'. 
    Or in wide format, where each Trait group 
    is a separate column with individual traits displayed as categories.
    For the Vieira dataset traits are displayed in wideformat 
    with trait groups as columns and traits as categories, 
    or if \code{"binary"} is specified, catgeorical traits are transformed to binary coding. Setting \code{format}. 
    Currently, for the Vieira database, all information that is not categorical and that
    is which is rather a comment than a trait is removed for now.
    This inlcudes e.g. information on ecological preferences (02),
    water body types, and development patterns.
    to \code{"wide"} and \code{database} to \code{"Vieira"} does not affect 
    the data processing of the \code{"Vieira"} trait database.
    If \code{"binary"} is specified, column names
    are named like: 'TraitGroup_individualtrait' (e.g. Voltinism_abbrev_Semivoltine)

    \code{meta} Currently returns trait definitions for the
    Conus trait database. 
    If \code{meta} is set \code{TRUE} than the Conus trait database 
    and the trait definitions are returned as a list object.
}
\examples{
\dontrun{
Conus_DB  <- getInvertTraitData(
    database = "Conus", 
    binary = TRUE, 
    meta = FALSE)
}
}