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
                  "sdOdeModel")
  aren <- expect_is(sdLoadModel("Arenstorf", repository = TRUE), 
                    "sdOdeModel")
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

# Testes de Classe de Equivalencia
test_that("Coupled model valid connections", code =
{
  # define a static model example and verify it
  staticM <- expect_is(sdStaticModel(id = "SM",
                                     defaultScenario = sdScenario(id = "Default",
                                                                  input = list(x = 1,
                                                                               y = 2),
                                                                  constant = list(z = 3),
                                                                  times = list(from = 0, to = 10, by = 1)),
                                     algebraicEquations = list(eq1 = "inp$x^2",
                                                               eq2 = "ct$z * inp$y")),
                       "sdStaticModel")
  expect_true(staticM$verifyModel())
  
  # define a ode model example and verify it
  odeM <- expect_is(sdOdeModel(id = "OM",
                               defaultScenario = sdScenario("Default",
                                                            state = list(a = 1),
                                                            input = list(b = 2),
                                                            constant = list(c = 3),
                                                            times = list(from = 0, to = 10, by = 1)),
                               aux = list(a1 = "inp$b * 3",
                                          a2 = "st$a * ct$c"),
                               DifferentialEquations = function(t, st, ct, par, inp, sw, aux)
                               {
                                 da <- st$a * inp$b/1000
                                 return(list(da))
                               }),
                    "sdOdeModel")
  expect_true(odeM$verifyModel())
  
  # define a coupled model example with one valid connection
  coupledM <- expect_is(sdCoupledModel(id = "CPM",
                                       components = list(staticM, odeM),
                                       connections = list(c("con1", "SM", "x", "OM", "aux$a2"),
                                                          c("con2", "SM", "y", "OM", "st$a"),
                                                          c("con3", "OM", "b", "SM", "eq$eq1"))),
                        "sdCoupledModel")
  expect_true(coupledM$buildCoupledModel(from = 0, to = 10, by = 1), info = "Test cases 1, 2 e 3")
  expect_true(coupledM$verifyModel(), info = "Verify test cases 1, 2 e 3")
  
  expect_silent(coupledM$addConnection(c("con4", "SM", "y", "OM", "st$a")))
  expect_warning(coupledM$buildCoupledModel(), info = "Test cases 4")
  expect_silent(coupledM$removeConnection("con4")) # removing redudant test case 4
  
  expect_warning(coupledM$addConnection(c("con5", "NA", "NA", "NA")), info = "Test case 5")
  
  expect_silent(coupledM$addConnection(c("con6", "NA", "y", "OM", "st$a")))
  expect_warning(coupledM$buildCoupledModel(), info = "Test cases 6")
  expect_silent(coupledM$removeConnection("con6")) # removing invalid test case 6
  
  expect_silent(coupledM$addConnection(c("con7", "SM", "y", "NA", "st$a")))
  expect_warning(coupledM$buildCoupledModel(), info = "Test cases 7")
  expect_silent(coupledM$removeConnection("con7")) # removing invalid test case 7
  
  expect_silent(coupledM$addConnection(c("con8", "SM", "NA", "OM", "st$a")))
  expect_warning(coupledM$buildCoupledModel(), info = "Test cases 8")
  expect_silent(coupledM$removeConnection("con8")) # removing invalid test case 8
  
  expect_silent(coupledM$addConnection(c("con9", "SM", "y", "OM", "st$b")))
  expect_warning(coupledM$buildCoupledModel(), info = "Test cases 9")
  expect_silent(coupledM$removeConnection("con9")) # removing invalid test case 9
  
  expect_warning(coupledM$addConnection(c("con10", "SM", "y", "OM", "b")), info = "Test cases 10")
  
  expect_warning(coupledM$addConnection(c("con11", "SM", "y", "OM", "NA$b")), info = "Test cases 11")
})