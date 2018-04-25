context("sdsim Messages")

test_that("auxiliary.R messages", code = 
{
  expect_warning(sdInitEquations(list(2)), info = "auxiliaryMsg$sdInitEq")
  #expect_warning(sdInitEquations(list("2")), info = "auxiliaryMsg$sdInitEq1")
  
})