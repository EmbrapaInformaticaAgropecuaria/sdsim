library(testthat)
context("Scenario instantiation")

test_that("Scenario object", code = 
{
  scen <- expect_is(sdScenario(
    id = "test",
    state = c(a = 2, b = 3),
    switch = data.frame(Variable = c("s"),
                        Value = c(1),
                        Description = c("test switch"))),
    "sdScenario")
  expect_null(scen$addConstant(c = 1))
  expect_null(scen$addInput(i = data.frame(c(1,2,3), c(5,10,15)), 
                            interpolation = c(i = "linear")))
  expect_type(scen$input$fun_$i, "closure")
  expect_null(scen$addParameter(p = 1))
  expect_type(scen$unit <- list(a = "state test"), "list")
  expect_equal(scen$setTimeSequence(0,10,1), 1) #equal the by
  expect_equal(scen$method <- "rk4", "rk4")
  scendf <- expect_type(scen$buildDataFrames(), "list")
  expect_equal(length(scendf), 6)
})