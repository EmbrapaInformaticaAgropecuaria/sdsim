library(testthat)
library(sdsim)

#test_check("sdsim")
## Atomic model
expect_warning(sdAtomicModel(), "sdsim::id - Missing model ID: It was set to 'sdAtomicModel2018-04-11'",
               info = "empty model with no id")
expect_is(sdAtomicModel(id = "test", 
                        description = "test test",
                          DifferentialEquations = function(t, st, ct, par, inp, sw, aux)
                          {
                            return(list(c(st$a*inp$pow)))
                          },
                          defaultScenario = sdScenario("Default",
                                                       times = c(from = 0, to = 2, by = 1),
                                                       state = c(a=1),
                                                       input = c(pow = 2),
                                                       method = "rk4")),
            "sdAtomicModel")
m <- sdAtomicModel(id = "test", 
                   description = "test test",
             DifferentialEquations = function(t, st, ct, par, inp, sw, aux)
             {
               return(list(c(st$a*inp$pow)))
             },
             defaultScenario = sdScenario("Default",
                                          times = c(from = 0, to = 2, by = 1),
                                          state = c(a=1),
                                          input = c(pow = 2),
                                          method = "rk4"))
scen_ob <- sdScenario("testScen", state = c(a = 2))
scen_ob2 <- sdScenario("testScen", input = c(pow = 3))


out <- sdSimulate(m)
out2 <- sdSimulate(m, scen_ob)

## Scenario Object
expect_warning(sdScenario(), "sdsim::id - Invalid scenario ID type: The scenario ID must be a string. It was set to 'scenario 2018-04-11'.",
               info = "empty scenario with no id")
expect_is(sdScenario("test id"), "sdScenario")

#regenerate the repository models
for (i in 1:5)
{
  m <- sdLoadModel(sdRepository()[[i]], repository = T)
  # m$saveToXml(paste0(sdRepository()[[i]],".xml"))
}