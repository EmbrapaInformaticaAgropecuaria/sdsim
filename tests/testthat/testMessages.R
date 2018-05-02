context("sdsim Messages")

test_that("auxiliary.R messages", code = 
{
  expect_warning(sdInitEquations(list(2)), info = "auxiliaryMsg$sdInitEq")
  expect_warning(sdInitEquations(list("2")), info = "auxiliaryMsg$sdInitEq1")
  expect_warning(sdInitEquations(list(e1 = "eq$e1", e2 = "eq$e1"), 
                                 eqName = "eq"),
                 info = "auxiliaryMsg$topologicalSortEq")
  expect_warning(sdTemporalFunction(x = "lala"), 
                 info = "auxiliaryMsg$sdTemporalFunction1")
  expect_warning(sdTemporalFunction(x = "lala"), 
                 info = "auxiliaryMsg$sdTemporalFunction1")
  expect_warning(sdTemporalFunction(x = "testAuxiliary.R"), 
                 info = "auxiliaryMsg$sdTemporalFunction2")
  expect_warning(sdTemporalFunction(x = TRUE), 
                 info = "auxiliaryMsg$sdTemporalFunction3")
  expect_warning(sdTemporalFunction(x = 2, method = T), 
                 info = "auxiliaryMsg$sdTemporalFunction4")
  expect_warning(sdTemporalFunction(x = 2, method = T), 
                 info = "auxiliaryMsg$sdTemporalFunction4")
  expect_warning(sdTemporalFunction(x = data.frame(c(c(1,3,2), c(4,5,10)))), 
                 info = "auxiliaryMsg$sdTemporalFunction6")
})

test_that("sdLoadModel", code = 
{
  expect_error(sdLoadModel(), info = "constructorsMsg$sdLoadModel1")
  expect_error(sdLoadModel("la", repository = T), 
               info = "constructorsMsg$sdLoadModel2")
  expect_error(sdLoadModel("la"), 
               info = "constructorsMsg$sdLoadModel3")
  expect_error(sdLoadModel("testAuxiliary.R"), 
               info = "constructorsMsg$sdLoadModel4")
})

test_that("sdLoadScenario", code = 
{
  expect_error(sdLoadScenario(""), info = "constructorsMsg$sdLoadScenario1")
  expect_error(sdLoadScenario(file = "testAuxiliary.R"), 
               info = "constructorsMsg$sdLoadScenario4")
})

test_that("sdCoupledModel.R", code =
{
  expect_error(sdBuildCoupledScenario(scenarios = list(2,3)), 
               info = "sdCoupledModelMsg$sdBuildCoupledScenario1")
  expect_warning(sdBuildCoupledScenario(scenarios = list(a = 2)),
                 info = "sdCoupledModelMsg$sdBuildCoupledScenario2")
  
  expect_warning(sdCoupledModel("test", components = sdOdeModel("test")), 
                 info = "sdCoupledModelMsg$addComponent0")
  expect_warning(sdCoupledModel("teste", components = c(sdOdeModel("test"), 
                                                        sdStaticModel("test"))), 
                 info = "sdCoupledModelMsg$addComponent1")
  expect_warning(sdCoupledModel("test", components = c(2)), 
                 info = "sdCoupledModelMsg$addComponent2")
  
  expect_warning(sdCoupledModel("test")$removeComponent("test"), 
                 info = "sdCoupledModelMsg$removeComponent")
  
  expect_warning(sdCoupledModel("test", connections = list(c(2,3))), 
                 info = "sdCoupledModelMsg$addConnection1")
  expect_warning(sdCoupledModel("test", connections = list(c(1,2,3,4,5))), 
                 info = "sdCoupledModelMsg$addConnection2")
  expect_warning(sdCoupledModel("test", connections = list(c(1,2,3,4,"aux$5"), 
                                                           c(1,2,3,4,"aux$5"))), 
                 info = "sdCoupledModelMsg$addConnection3")
  
  expect_warning(sdCoupledModel("test", connections = 
                                  c(1,2,3,4,"aux$5"))$removeConnection(2), 
                 info = "sdCoupledModelMsg$removeConnection")
  
  expect_error(sdCoupledModel("test")$verifyModel(), 
                 info = "sdCoupledModelMsg$verifyModel1")
  
  expect_error(sdCoupledModel("test")$buildCoupledModel(), 
               info = "sdCoupledModelMsg$buildCoupledModel1")
  
  expect_warning(sdCoupledModel("test")$defaultScenario, 
               info = "sdCoupledModelMsg$defaultScenario")
  
  expect_warning(sdCoupledModel("test")$eqConnections, 
               info = "sdCoupledModelMsg$connectionsList")
  expect_warning(sdCoupledModel("test")$stConnections, 
                 info = "sdCoupledModelMsg$connectionsList")
  
  expect_warning(sdCoupledModel("test")$indexComponents, 
                 info = "sdCoupledModelMsg$indexComponents")
})

test_that("sdModel.R", code =
{
  expect_warning(sdOdeModel(), info = "sdModelMsg$id1")
  expect_warning(sdOdeModel(1), info = "sdModelMsg$id2")
  expect_warning(sdOdeModel("aux"), info = "sdModelMsg$id3")
  
  expect_warning(sdOdeModel("test", description = 3), 
                 info = "sdModelMsg$description")
})

test_that("sdOdeModel.R", code =
{
  expect_warning(sdOdeModel("test", 
                            DifferentialEquations = function(x) return(x)),
                 info = "sdOdeModelMsg$initialize1")
  expect_warning(sdOdeModel("test", 
                            InitVars = function(x) return(x)),
                 info = "sdOdeModelMsg$initialize2")
  expect_warning(sdOdeModel("test", 
                            PostProcessVars = function(x) return(x)),
                 info = "sdOdeModelMsg$initialize3")
  expect_warning(sdOdeModel("test", 
                            RootSpecification = function(x) return(x)),
                 info = "sdOdeModelMsg$initialize4")
  expect_warning(sdOdeModel("test", 
                            EventFunction = function(x) return(x)),
                 info = "sdOdeModelMsg$initialize5")
  expect_warning(sdOdeModel("test", aux = 2),
                 info = "sdOdeModelMsg$initialize7")
  expect_warning(sdOdeModel("test", globalFunctions = list(a =2)),
                 info = "sdOdeModelMsg$initialize8")
  expect_warning(sdOdeModel("test", globalFunctions = list(2)),
                 info = "sdOdeModelMsg$initialize9")
  expect_warning(sdOdeModel("test", aux = list(st ="2")),
                 info = "sdOdeModelMsg$initialize10")
  
  expect_error(sdOdeModel("id")$verifyModel(),
                 info = "sdOdeModelMsg$verifyModel0 ")
  
  
})







