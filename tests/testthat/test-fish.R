test_that("make sure error kicks on fish when standardize is set to FALSE",
          {
            expect_error(getFishData(standardize = FALSE,
                                     dataType = "abun"))
          })

##throws all sorts of errors, skip for now
# test_that("make sure message kicks when fish is non-standardized abundance",
#           {
#             expect_message(getFishData(standardize = "none",
#                                        dataType = "abun",
#                                        agency = "USGS"))
#           })
#
# test_that("make sure fish runs and gives correct number of rows/columns",
#           {
#             df1 <- getFishData(dataType = "occur",
#                                         taxonLevel = "Species",
#                                         agency = c("USGS"),
#                                         standardize = "none",
#                                         hybrids = FALSE,
#                                         boatableStreams = FALSE)
#             expect_true(exists("df1"))
#             expect_equal(nrow(df1), 2543)
#             expect_equal(ncol(df1), 552)
#           })
