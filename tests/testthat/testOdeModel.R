context("Atomic Model instantiation and simulation")

test_that("Empty atomic model objet", code =
{
  m <- expect_warning(sdOdeModel(),
                      info = "empty model with no id")
  expect_error(m$verifyModel())

  expect_error(sdSimulate(m))

  expect_is(m$saveXml(), "XMLInternalElementNode")
})


test_that("Not empty atomic model object", code =
{
  scen <- expect_is(sdScenario("Default",
                     times = c(from = 0, to = 2, by = 1),
                     state = c(a=1),
                     input = c(pow = 2),
                     method = "rk4"),
                    "sdScenario")
  m <- expect_is(sdOdeModel(id = "test",
                               description = "test test",
                               DifferentialEquations = function(t, st, ct, par, inp, sw, aux)
                               {
                                 return(list(c(st$a*inp$pow)))
                               },
                               defaultScenario = scen),
                 "sdOdeModel")
  expect_true(m$verifyModel())
  expect_is(sdSimulate(m), "sdOutput")
})

test_that("Atomic model with Global Funs", code =
{
  scen <- expect_is(sdScenario("Default",
                               times = c(from = 0, to = 2, by = 1),
                               state = c(a=1),
                               input = c(pow = 2),
                               method = "rk4"),
                    "sdScenario")
  m <- expect_is(sdOdeModel(id = "test",
                               description = "test test",
                               DifferentialEquations = function(t, st, ct, par,
                                                                inp, sw, aux)
                               {
                                 return(list(c(a(st$a)*inp$pow)))
                               },
                               defaultScenario = scen,
                               globalFunctions = list(a = function(x) {
                                 2*x}),
                               aux = list(b = "a(1)")),
                 "sdOdeModel")
  expect_true(m$verifyModel())
  expect_is(sdSimulate(m), "sdOutput")
})