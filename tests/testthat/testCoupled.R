context("Coupled Model instantiation and simulation")

test_that("Empty coupled model objet", code =
{
  m <- expect_is(sdCoupledModel("id"), "sdCoupledModel")
  expect_error(m$buildCoupledModel())
  expect_error(sdSimulate(m))
  expect_warning(m$addComponent(2))
  expect_warning(m$removeComponent(2))

  expect_warning(m$addConnection(c(1, 2, 3, 4), c(1,2,3,4,"st$a")))

  expect_warning(m$addConnection(c(2, 2, 3, 4, "a"), c(1,2,3,4,"st$a")))

  expect_warning(m$removeConnection(4))
})

# test connection with state var
# test con with aux eq
# test con with algebraic eq

test_that("Coupled model connections", code = 
{
  scen <- expect_is(sdScenario("Default",
                               times = c(from = 0, to = 2, by = 1),
                               parameter = data.frame(Variable = c("v"),
                                                      Value = c(1),
                                                      Description = c("test")),
                               input = c(pow = 2, heightcon = 0, D1con = 0),
                               method = "rk4"),
                    "sdScenario")
  mstatic <- expect_is(sdStaticModel(id = "test",
                               description = "test test",
                               algebraicEquations = list(eq1 = "par$v^inp$pow",
                                                         eq2 = "eq$eq1*2*eq$eq3",
                                                         eq3 = "b(par$v)",
                                                         heightcon = "inp$heightcon",
                                                         D1con = "inp$D1con"),
                               defaultScenario = scen,
                               globalFunctions = list(b = function(x)
                               { return(2*x)})),
                 "sdStaticModel")
  bb <- expect_is(sdLoadModel("BouncingBall", repository = TRUE), 
                  "sdAtomicModel")
  aren <- expect_is(sdLoadModel("Arenstorf", repository = TRUE), 
                    "sdAtomicModel")
  aren$defaultScenario$input$testconEq <- 0
  aren$initialize(id = "arenstorf", aux = c(aren$aux, testconEq = "inp$testconEq"))
  
  cm <- expect_is(sdCoupledModel("testCons", components = c(mstatic, bb, aren),
                       connections = list(
                         c("conEq", "arenstorf", "testconEq", "test", "eq$eq2"),
                         c("conAux", "test", "D1con", "arenstorf", "aux$D1"),
                         c("conSt", "test", "heightcon", "BouncingBall", "st$height")
                       )), "sdCoupledModel")
  expect_true(cm$buildCoupledModel(0, 50, 0.5, method = "lsoda"))
  expect_true(cm$verifyModel())
  outcm <- expect_is(sdSimulate(cm), "sdOutput")
  
  # check connection result
  expect_true(all(outcm$auxTrajectory$arenstorf.testconEq == outcm$outTrajectory$test.eq2))
  expect_true(all(outcm$auxTrajectory$arenstorf.D1 == outcm$outTrajectory$test.D1con))
  expect_true(all(outcm$outTrajectory$BouncingBall.height == outcm$outTrajectory$test.heightcon))
})
