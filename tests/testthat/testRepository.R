context("Model repository")
library(sdsim)

test_that("Loading atomic models", code = 
{
  for (i in 1:5)
  {
    m <- expect_is(sdLoadModel(sdRepository()[[i]], repository = T), 
              "sdAtomicModel")
    expect_true(m$verifyModel())
  }
})
