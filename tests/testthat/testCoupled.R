library(testthat)
context("Coupled Model instantiation and simulation")

test_that("Empty coupled model objet", code = 
{
  m <- expect_is(sdCoupledModel("id"), "sdCoupledModel")
})
