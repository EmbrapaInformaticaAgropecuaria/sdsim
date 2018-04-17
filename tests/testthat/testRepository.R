library(testthat)
context("Model repository")

test_that("Loading atomic models", code = 
{
  for (i in 1:5)
  {
    m <- expect_is(sdLoadModel(sdRepository()[[i]], repository = T), 
              "sdModel")
    expect_true(m$verifyModel())
    expect_is(sdSimulate(m), "sdOutput")
  }
})
