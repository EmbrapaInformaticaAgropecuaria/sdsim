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
                               modelDynamics = function(t, st, ct, par, inp, sw, aux)
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

# Testes de Casos de Uso
test_that("Ode model with Root Specification", code =
{
  # Primeira caso de uso -  Root especificda com vetor numerico
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
                            defaultScenario = expect_is(sdScenario("Default",
                                                                   times = c(from = 0, to = 5, by = 1),
                                                                   state = c(a = 1), input = c(pow = 2), 
                                                                   method = "rk4"), "sdScenario")), "sdOdeModel")
  outvec <- expect_warning(sdSimulate(m), info = "test if the method has root-finding capability")
  m$defaultScenario$method <- "lsoda"
  
  # Segunda caso de uso - Root especificada com funcao
  m$initialize("fun root", RootSpecification = function(t, st, ct, par, inp, sw, aux)
  {
    if (t %in% c(1,3))
      return(0)
    else
      return(1)
  })
  outfun <- expect_is(sdSimulate(m), "sdOutput")
  
  # Terceira caso de uso - Root especificada com data.frama
  m$initialize("df root", RootSpecification = data.frame(
    var = c("a", "a"),
    time = c(1, 3),
    value = c(2, 2),
    method = c("multiply", "multiply")))
  outdf <- expect_is(sdSimulate(m), "sdOutput")
  
  outCorrectVec <- ode(y = c(a = 1), 
                       parms = c(pow = 2), 
                       times = seq(from = 0, to = 5, by = 1),
                       func = function(t, y, parms)
                       {
                         dy <- y*parms[['pow']]
                         return(list(dy))
                       },
                       method = "lsoda",
                       events = list(func = function(t, y, parms)
                       {
                         y <- y*2
                         return(y)
                       }, time = c(1, 3)))
  
  outCorrectFun <- ode(y = c(a = 1), 
                       parms = c(pow = 2), 
                       times = seq(from = 0, to = 5, by = 1),
                       func = function(t, y, parms)
                       {
                         dy <- y*parms[['pow']]
                         return(list(dy))
                       },
                       method = "lsoda",
                       events = list(func = function(t, y, parms)
                       {
                         y <- y*2
                         return(y)
                       }, root = TRUE),
                       rootfun = function(t, y, parms)
                       {
                         if (t %in% c(1,3))
                           return(0)
                         else
                           return(1)
                       })
  
  outCorrectDf <- ode(y = c(a = 1), 
                      parms = c(pow = 2), 
                      times = seq(from = 0, to = 5, by = 1),
                      func = function(t, y, parms)
                      {
                        dy <- y*parms[['pow']]
                        return(list(dy))
                      },
                      method = "lsoda",
                      events = list(data = data.frame(
                        var = c("a", "a"),
                        time = c(1, 3),
                        value = c(2, 2),
                        method = c("multiply", "multiply"))))
  
  # teste de validade dos casos de testes
  expect_true(all(outdf$outTrajectory == outCorrectDf))
  expect_true(all(outfun$outTrajectory == outCorrectFun))
  expect_true(all(outvec$outTrajectory == outCorrectVec))
})
