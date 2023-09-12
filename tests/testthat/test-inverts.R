test_that("make sure error kicks on inverts when dataType is set to capital Occur",
          {
            expect_error(getInvertData(dataType = "Occur"))
          })

##need to figure these out.
# test_that("make sure warning kicks when inverts is non-rarefied occurence",
#           {
#             expect_warning(getInvertData(rarefy = FALSE,
#                                          dataType = "occur",
#                                          agency = "USGS"))
#           })

# test_that("make sure fish runs and gives correct number of rows/columns",
#           {
#             df2 <- getInvertData(dataType = "occur",
#                                  taxonLevel = "Genus",
#                                  agency = "USGS",
#                                  lifestage = FALSE,
#                                  rarefy = TRUE,
#                                  seed = 1,
#                                  boatableStreams = FALSE)
#             expect_true(exists("df2"))
#             expect_equal(nrow(df2), 4022)
#             expect_equal(ncol(df2), 613)
#           })
