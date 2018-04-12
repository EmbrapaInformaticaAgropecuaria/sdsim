context("Atomic Model instantiation and simulation")
library(sdsim)

test_that("Empty atomic model objet", code = 
{
  m <- expect_warning(sdAtomicModel(), 
                      paste0("sdsim::id - Missing model ID: It was set to ", 
                             "'sdAtomicModel", Sys.Date(), "'"),
                      info = "empty model with no id")
  expect_error(m$verifyModel(), 
               paste0("sdsim::verifyModel - Model '", m$id, 
                      "' ODE validation aborted: No differential equations ", 
                      "function was set."))
  
  expect_error(sdSimulate(m), 
               paste0("sdsim::sdSimulate - Simulation of the model '", m$id,
                      "' aborted: No differential equations function, model is empty."))
  
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
  m <- expect_is(sdAtomicModel(id = "test", 
                               description = "test test",
                               DifferentialEquations = function(t, st, ct, par, inp, sw, aux)
                               {
                                 return(list(c(st$a*inp$pow)))
                               },
                               defaultScenario = scen),
                 "sdAtomicModel")
  expect_true(m$verifyModel())
  expect_is(sdSimulate(m), "sdOutput")
})



