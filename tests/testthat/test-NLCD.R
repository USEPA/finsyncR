test_that("confirm that function works", {
  dat = data.frame(SiteNumber = "USGS-05276005",
                   CollectionYear = 2007)
  dfNLCD <- getNLCDData(data=dat,
                        scale = "Cat",
                        group = FALSE)

  expect_true(exists("dfNLCD"))
  expect_equal(nrow(dfNLCD), 1)
  expect_equal(ncol(dfNLCD), 17)
})
