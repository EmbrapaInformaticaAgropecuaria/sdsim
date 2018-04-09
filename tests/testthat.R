library(testthat)
library(sdsim)

#test_check("sdsim")

m <- sdModel(modelId = "test", 
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
