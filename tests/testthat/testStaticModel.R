context("Static Model instantiation and simulation")

test_that("Not empty static model object", code =
{
  scen <- expect_is(sdScenario("Default",
                               times = c(from = 0, to = 2, by = 1),
                               state = c(a=1),
                               parameter = data.frame(Variable = c("v"),
                                                      Value = c(1),
                                                      Description = c("test")),
                               input = c(pow = 2),
                               method = "rk4"),
                    "sdScenario")
  m <- expect_warning(expect_is(sdStaticModel(id = "test",
                               description = "test test",
                               algebraicEquations = list(eq1 = "par$v^inp$pow",
                                                         eq2 = "eq$eq1*2*eq$eq3",
                                                         eq3 = "par$v"),
                               defaultScenario = scen),
                 "sdStaticModel"))
  expect_true(m$verifyModel())
  expect_is(sdSimulate(m), "sdOutput")
})

test_that("Static model object with global funs", code =
{
  scen <- expect_is(sdScenario("Default",
                               times = c(from = 0, to = 2, by = 1),
                               parameter = data.frame(Variable = c("v"),
                                                      Value = c(1),
                                                      Description = c("test")),
                               input = c(pow = 2),
                               method = "rk4"),
                    "sdScenario")
  m <- expect_is(sdStaticModel(id = "test",
                               description = "test test",
                               algebraicEquations = list(eq1 = "par$v^inp$pow",
                                                         eq2 = "eq$eq1*2*eq$eq3",
                                                         eq3 = "b(par$v)"),
                               defaultScenario = scen,
                               globalFunctions = list(b = function(x)
                                 { return(x)})),
                 "sdStaticModel")
  expect_true(m$verifyModel())
  expect_is(sdSimulate(m), "sdOutput")
})
