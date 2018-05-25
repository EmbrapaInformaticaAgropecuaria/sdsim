context("Ode Model instantiation and simulation")

test_that("Empty Ode model objet", code =
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

test_that("Ode model with Global Funs", code =
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

test_that("Ode model with Root Specification", code =
{
  scen <- expect_is(sdScenario("Default",
                               times = c(from = 0, to = 5, by = 1),
                               state = c(a = 1), input = c(pow = 2), 
                               method = "rk4"), "sdScenario")
  m <- expect_is(sdOdeModel(id = "vec root",
                            description = "test test",
                            DifferentialEquations = function(t, st, ct, par,
                                                             inp, sw, aux)
                            {
                              return(list(c(st$a*inp$pow)))
                            },
                            RootSpecification = c(1, 3),
                            EventFunction = function(t, st, ct, par,
                                                     inp, sw, aux)
                            {
                              st$a <- st$a*2
                              return(st)
                            },
                            defaultScenario = scen), "sdOdeModel")
  outvec <- expect_warning(sdSimulate(m), info = "root method")
  
  m$initialize("fun root", RootSpecification = function(t, st, ct, par, inp, sw, aux)
  {
    if (t %in% c(1,3))
      return(0)
    else
      return(1)
  })
  m$defaultScenario$method <- "lsoda"
  outfun <- expect_is(sdSimulate(m), "sdOutput")
  
  m$initialize("df root", RootSpecification = data.frame(
    var = c("a", "a"),
    time = c(1, 3),
    value = c(2, 2),
    method = c("multiply", "multiply")))
  outdf <- expect_is(sdSimulate(m), "sdOutput")
  expect_true(all(outfun$outTrajectory == outvec$outTrajectory && 
                  outfun$outTrajectory == outdf$outTrajectory))
})
