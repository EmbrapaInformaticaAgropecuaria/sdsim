# List of warnings and errors of all the sdsim package files

## FILE: auxiliary.R
## 
auxiliaryMsg <- list()

# use case:
# sdInitEquations(list(2,3))
# auxiliaryMsg$sdInitEq <- function(eqName, eq)
#   warning("sdsim::sdInitEquations - Initializing equations: The ", eqName,
#           " equations argument must be a ",
#           "named list of R-expressions and/or strings with equations in ",
#           "R-format. The element of type ", typeof(eq),
#           " will be skipped.", call. = F)
# TODO: Vitor
auxiliaryMsg$sdInitEq <- paste0(
  "sdsim::sdInitEquations - Initializing equations: The %s ",
  "equations argument must be a ",
  "named list of R-expressions and/or strings with equations in ",
  "R-format. The element of type %s ",
  "will be skipped."
)

auxiliaryMsg$sdInitEq1 <- paste0("sdsim::sdInitEquations - Initializing ", 
          "equations: The '%s' equations argument must be a ",
          "named list of R-expressions and/or strings with equations in ",
          "R-format. The unnamed element with value '%s' will be skipped.")

# use case:
# sdInitEquations(list(e1 = "eq$e1", e2 = "eq$e1"), eqName = "eq")
auxiliaryMsg$topologicalSortEq <- function(eqName, equations, eqorder, 
                                           dependents)
  warning("sdsim::sdInitEquations - Sorting Equations: Could not sort the ", 
          eqName, " equations. Refactor your model to remove the circular ",
          "dependencies. An empty list was returned because the following ",
          "equations could not be ordered: ", 
          paste(names(equations)[!(names(equations) %in% eqorder)], 
                collapse = ", "), "\nEquations dependents:\n",
          paste0(utils::capture.output(dependents[
            names(equations)[!(names(equations) %in% eqorder)] %in% 
              names(dependents)]), 
            collapse = "\n"), call. = F)

auxiliaryMsg$topologicalSortEq <- paste0(
  "sdsim::sdInitEquations - Sorting Equations: Could not sort the %s",
  " equations. Refactor your model to remove the circular ",
  "dependencies. An empty list was returned because the following ",
  "equations could not be ordered: %s\nEquations dependents:\n%s"
)

auxiliaryMsg$sdTemporalFunctionList <- paste0(
  "sdsim::sdTemporalFunctionList - Error Transforming the time series ",
  "variables: The size of the list 'x' must match the size of the list ",
  "'methods'. Transformation aborted, returned NULL."
  )

# case use:
# sdTemporalFunction(x = "lala")
auxiliaryMsg$sdTemporalFunction1  <- paste0( 
  "sdsim::sdTemporalFunction - Error: The time series file '%s' ", 
          " does not exist. Use the argument ",
          "'timeSeriesDirectory' to set the right path to the time series ", 
          "files. Temporal function transformation aborted, returned NULL."
  )

# case use:
# sdTemporalFunction(x = "DESCRIPTION")

auxiliaryMsg$sdTemporalFunction2  <- paste0( 
  "sdsim::sdTemporalFunction - Error opening the time series file '%s'.", 
          " The default separator is ',' and the default decimal point is '.'.",
          " Temporal function transformation aborted, returned NULL. %s"
          
)

# case use:
# sdTemporalFunction(x = TRUE)
auxiliaryMsg$sdTemporalFunction3  <- paste0( 
  "sdsim::sdTemporalFunction - Wrong time series variable 'x' type '%s':",
          " Please use one of the following types: fixed numeric",
          " value, matrix, data.frame or character (text file name).",
          " Transformation aborted, returned NULL."
 )

# case use:
# sdTemporalFunction(x = 2, method = T)

auxiliaryMsg$sdTemporalFunction4  <- paste0(
  "sdsim::sdTemporalFunction - The given interpolation method is not", 
          " supported: See help('sdTemporalFunction') for the supported", 
          " methods. Transformation aborted, returned NULL."
)

auxiliaryMsg$sdTemporalFunction5  <- paste0(
  "sdsim::sdTemporalFunction - Error interpolating the time series ", 
          "data points: Transformation aborted, returned NULL. %s"
)

auxiliaryMsg$sdTemporalFunction6 <- paste0(
  "sdsim::sdTemporalFunction - Error in the time serie data points: Missing ",
  "the times or the value column. Transformation aborted, returned NULL. ")

auxiliaryMsg$sdMakeFlows1 <- paste0(
  "sdsim::sdMakeFlows - Error initializing flow: Argument 'flows' has ",
  "value 'NULL'. Must be an Array or List.")

auxiliaryMsg$sdMakeFlows2 <- paste0(
  "sdsim::sdMakeFlows - Error initializing flow:Argument 'flow_rate' has value ",
  "'NULL'. Must be an Array or List.")

auxiliaryMsg$sdMakeFlows3 <- paste0(
  "sdsim::sdMakeFlows - Error initializing flow: No elements identified in ",
  "argument 'flows'. Must be a non-empty Array or List. ")

auxiliaryMsg$sdMakeFlows4 <- paste0(
  "sdsim::sdMakeFlows - Error initializing flow: No elements identified in ",
  "argument 'flow_rate'. Must be a non-empty Array or List. ")

auxiliaryMsg$sdMakeFlows5 <- paste0(
  "sdsim::sdMakeFlows - Error initializing flow: The length of the argument ",
  "'flows' does not match the lenght of the argument 'flow_rate'.")

auxiliaryMsg$sdMakeFlows6 <- paste0(
  "sdsim::MakeFlows - The variable '%s' in argument 'flows' ",
  "does not match a boundary or st variable.")

auxiliaryMsg$sdOdeClass1 <- paste0(
  "sdsim::OdeClass - Error initializing differential ",
  "equations: No elements identified in argument 'ode'. Must ",
  "be a non-empty List.")

auxiliaryMsg$sdOde2 <- paste0(
  "sdsim::odeClass - Error initializing differential ",
  "equations: Argument 'ode' has value 'NULL'. Must be an ",
  "Array or List.")

# case use:
# bb <- sdLoadModel("BouncingBall", repository = T)
# sdSimulate(bb, scenario = sdScenario("", parameter = c(a = 2)))
# auxiliaryMsg$MergeLists <- function(x, listName)
#   warning("sdsim::MergeLists - The variable '", x, "' from the scenario ", 
#           listName, " list does not exist in the model's default scenario.", 
#           call. = F)

## FILE: constructors.R
##
constructorsMsg <- list()

# case use:
# sdLoadModel()
constructorsMsg$sdLoadModel1 <- paste0(
  "sdsim::sdLoadModel - Load Model aborted:",
  "\n  The Models Repository list: ",
  "\n  - %s",
  "\n\n  Use 'file = any of the above names' and 'repository = TRUE' to ",
  "load a model from the sdsim package repository.")

# case use:
# sdLoadModel("la", repository = T)
constructorsMsg$sdLoadModel2 <- paste0(
  "sdsim::sdLoadModel - Load Model aborted: The given model name '%s", 
  "' is not present in the repository.",
  "\n  The Models Repository list: ",
  "\n  - %s",
  "\n\n  Use 'file = any of the above names' and 'repository = T' to ",
  "load a model from the sdsim package repository.")

# case use:
# sdLoadModel("la")
constructorsMsg$sdLoadModel3 <- paste0(
  "sdsim::sdLoadModel - Load Model aborted: The given file '%s'", 
       " do not exists.")

# case use:
# sdLoadModel("DESCRIPTION")
constructorsMsg$sdLoadModel4 <- paste0(
  "sdsim::sdLoadModel - Load Model '%s':", 
       " The given file is not a valid XML. Use the package functions to ", 
       "generate it. See help('sdLoadModel') and in the desired model ", 
       "help pages look for the method '$saveXml'. %s")

# warning when reading file
constructorsMsg$sdLoadModel5 <- paste0(
  "sdsim::sdLoadModel - Load Model from file '%s': %s")

# prefix not valid
constructorsMsg$sdLoadModel6 <- paste0(
    "sdsim::sdLoadModel - Load Model aborted: The given file is not a ", 
       "valid XML file. Generate your XML files using the sdsim package ", 
       "functions. See help('sdLoadModel') and in the desired type of model ", 
       "help pages look for the method '$saveXml'.")

# valid xml without a model
constructorsMsg$sdLoadModel7 <- paste0(
  "sdsim::sdLoadModel - Load Model aborted: The given XML file do not ",
       "contain a model.")

# case use:
# sdLoadScenario(file = "")
constructorsMsg$sdLoadScenario1 <- paste0(
  "sdsim::sdLoadScenario - Load Scenario aborted: The given file '%s'", 
       " do not exist.")

# constructorsMsg$sdLoadScenario2 <- function(file, e = NULL)
constructorsMsg$sdLoadScenario2 <- paste0(
  "sdsim::sdLoadScenario - Load Scenario '%s' aborted: ",
  "The given file is not a valid XML. See help('sdLoadScenario') for ", 
  "the set of rules to encode a sdScenario in EXCEL file or use the ",
  "method '$saveXml' present in the help('sdScenarioClass') to generate",
  " a valid XML. %s"
)

constructorsMsg$sdLoadScenario3 <- paste0(
  "sdsim::sdLoadScenario - Load Scenario '%s': %s")

# CASE USE
# sdLoadScenario(file = "DESCRIPTION")
constructorsMsg$sdLoadScenario4 <- paste0(
  "sdsim::sdLoadScenario - Load Scenario '%s'", 
  " aborted: Wrong file extension. It should be a Extensible ", 
  "Markup Language (XML) or an EXCEL file. See help('sdLoadScenario') for",
  " the set of rules to encode a sdScenario.")

## FILE: readInputData.R
##
readInputDataMsg <- list()

readInputDataMsg$ConvertDataFrameToList <- paste0(
  "sdsim::sdLoadScenario - Load Scenario from EXCEL: an invalid ",
  "data.frame was skipped, probably with empty values. Refactor your ",
  "file if it is not wanted.")

readInputDataMsg$ReadDataExcel1 <- paste0(
  "sdsim::sdLoadScenario - Load Scenario from EXCEL: Error opening ", 
  "the file '%s'. %s")

readInputDataMsg$ReadDataExcel2 <- paste0(
  "sdsim::sdLoadScenario - Load Scenario from EXCEL: Warning opening ", 
  "the file '%s'. %s")

readInputDataMsg$LoadModelScenario1 <- paste0(
  "sdsim::sdLoadScenario - Loading scenario '%s'",
  " from EXCEL aborted: The scenario file path do not exist. ",
  "No variable will be loaded.")

## FILE sdCoupledModel.R
## 
sdCoupledModelMsg <- list()

# sdBuildCoupledScenario(scenarios = list(2,3))
sdCoupledModelMsg$sdBuildCoupledScenario1 <- paste0(
  "sdsim::sdBuildCoupledScenario - Error building the coupled scenario: ",
  "The 'scenarios' argument must be a list named with the component ID ",
  "that will use each scenario in the coupled model. ", 
  "At least one name was missing.")

# use case
# sdBuildCoupledScenario(scenarios = list(a = 2))
sdCoupledModelMsg$sdBuildCoupledScenario2 <- paste0(
  "sdsim::sdBuildCoupledScenario - Building Coupled Scenario: The ", 
  "argument 'scenarios' must be a named list of not empty ", 
  "sdScenario's objects or character file names. Scenario of type '%s'", 
  " of the component '%s'",
  " could not be added.")

# sdCoupledModel("test", components = sdOdeModel("test"))
sdCoupledModelMsg$addComponent0 <- paste0(
  "sdsim::addComponent - Coupled Model '%s",
  "' adding component: The component '%s",
  "' identification is equal to the coupled model ID. ",
  "Component skipped, refactor its ID and try to add it again.")

# sdCoupledModel("teste", components = c(sdOdeModel("test"), 
# sdStaticModel("test")))
sdCoupledModelMsg$addComponent1 <- paste0(
  "sdsim::addComponent - Coupled Model '%s",
  "' adding component: The component '%s",
  "' already exists in the coupled model. ",
  "It will be removed and overwritten.")

# sdCoupledModel("test", components = c(2))
sdCoupledModelMsg$addComponent2 <- paste0(
  "sdsim::addComponent - Coupled Model '%s' adding component: the argument ",
  "'...' must be a list of not empty sdModel's or character XML file names. ", 
  "Component of type '%s' could not be added.")

#sdCoupledModel("test")$removeComponent("test")
sdCoupledModelMsg$removeComponent <- paste0(
  "sdsim::removeComponent - Coupled Model '%s' removing component: The ", 
  "components '%s' do not exist in the coupled model and ",
  "thus can not be removed.")

# sdCoupledModel("test", connections = list(c(2,3)))
sdCoupledModelMsg$addConnection1 <- paste0(
  "sdsim::addConnection - Coupled Model '%s' adding connection: Each ", 
  "connection must be a vector with 5 elements. The connection: c(%s) ",
  "will be skipped.")

# sdCoupledModel("test", connections = list(c(1,2,3,4,5)))
sdCoupledModelMsg$addConnection2 <- paste0(
  "sdsim::addConnection - Coupled Model '%s'",
  " adding connection: The 5th element of the connection vector must ",
  "include the prefix st$, aux$ or eq$, indicating a state variable, ",
  "an auxiliary or an algebraic equation connection, respectively. ",
  "The connection: c(%s", 
  ") will be skipped.")

# sdCoupledModel("test", connections = list(c(1,2,3,4,"aux$5"), 
#c(1,2,3,4,"aux$5")))
sdCoupledModelMsg$addConnection3 <- paste(
  "sdsim::addConnection - Coupled Model '%s'",
  " adding connection: The connection '%s'", 
  " already exists in the coupled model. It will be overwritten.")

# sdCoupledModel("test", connections = c(1,2,3,4,"aux$5"))$removeConnection(2)
sdCoupledModelMsg$removeConnection <- paste0(
  "sdsim::removeConnection - Coupled Model '%s' removing connection: The ", 
  "connection ID '%s' do not exist in the coupled model and ",
  "thus can not be removed.")

# sdCoupledModel("test")$verifyModel()
sdCoupledModelMsg$verifyModel1 <- paste0(
    "sdsim::verifyModel - Coupled Model '%s'", 
       " verification error: Build the coupled model first using the method ",
       "'$buildCoupledModel'. Coupled model verification aborted.")

sdCoupledModelMsg$verifyModel2 <- paste0(
  "sdsim::verifyModel - Coupled Model '%s'",
  " verification: The given scenario must be of type list containing ",
  "multiple sdScenario objects named with the respective component ID,",
  " or of type sdScenarioClass containing a single coupled",
  " scenario object (see help('sdBuildCoupledScenario')).",
  " The given scenario of type '%s' will not be used.")

sdCoupledModelMsg$verifyModel3 <- paste0(
  "sdsim::verifyModel - Coupled Model '%s'",
  " verification: No time sequence informed. Define the time",
  " sequence in the default scenario and reset it.",
  " Initial time equal 0 will be used instead.")

sdCoupledModelMsg$verifyModel4 <- paste0(
  "sdsim::verifyModel - Coupled Model '%s'",
  " verification: Error evaluating the auxiliary equation %s. %s")

sdCoupledModelMsg$verifyModel5 <- paste0(
  "sdsim::verifyModel - Coupled Model '%s'",
  "verification: Evaluation of the auxiliary variable %s ",
  "may be incorrect. Value: %s."
)

sdCoupledModelMsg$verifyModel6 <- paste0(
  "sdsim::verifyModel - Coupled Model '%s' component '%s'",
  " verification: Error running the ode. %s")

sdCoupledModelMsg$verifyModel7 <- paste0(
  "sdsim::verifyModel - Coupled Model '%s'",
  " component '%s' verification: variable $%s", 
  " from the '%s' list in the $componentsEquations may be",
  " incorrect. It has '%s' value.")

sdCoupledModelMsg$verifyModel8 <- paste0(
  "sdsim::verifyModel - Coupled Model '%s'",
  " component '%s' verification: variable $%s", 
  " in the $componentsEquations may be incorrect. It has '%s'",
  " value.")

sdCoupledModelMsg$verifyModel9 <- paste0(
  "sdsim::verifyModel - Coupled Model '%s'",
  " verification: the first element of the coupled model",
  " definition return value should be a numeric vector",
  " containg the state variables derivatives.",
  " Wrong derivative return type: '%s'.")

sdCoupledModelMsg$verifyModel10 <- paste0(
  "sdsim::verifyModel - Coupled Model '%s", 
  "' verification: the number of derivatives returned by ", 
  " $componentsEquations (%s - %s",
  ") must equal the length of the initial conditions vector (%s).")

sdCoupledModelMsg$verifyModel11 <- paste0(
  "sdsim::verifyModel - Coupled Model '%s'", 
  " verification: the coupled model definition function should",
  " return a list. Wrong return type: '%s'.")

sdCoupledModelMsg$verifyModel12 <- paste0(
  "sdsim::verifyModel - Coupled Model '%s", 
          "' Ordinary Differential Equations verified.")

sdCoupledModelMsg$buildCoupledModel0 <- paste0(
  "sdsim::buildCoupledModel - Coupled model '%s' build error: ",
  "the components are empty.")

# sdCoupledModel("test")$buildCoupledModel()
sdCoupledModelMsg$buildCoupledModel1 <- paste0(
  "sdsim::buildCoupledModel - Coupled Model '%s' build error: no component was", 
  " added. Add a component before building the coupled model.")

sdCoupledModelMsg$buildCoupledModel2 <- paste0(
  "sdsim::buildCoupledModel - Coupled Model '%s' ", 
  " Error building the connection vectors: The model '%s' ", 
  " is not present in the coupled components. Refactor the",
  " connection to make it valid. Connection '%s' skipped.")

sdCoupledModelMsg$buildCoupledModel3 <- paste0(
  "sdsim::buildCoupledModel - Coupled Model '%s'", 
  " Error Building the connection vectors: The %s %s", 
  " from the model '%s' do not exist. Refactor",
  " the connection to make it valid. Connection '%s' skipped.")

sdCoupledModelMsg$buildCoupledModel4 <- paste0(
  "sdsim::buildCoupledModel - Coupled Model '%s'", 
  " Error Building the connection vectors: The",
  " connected output can only come from the auxiliary", 
  " equations list (aux$<varName>), from the state",
  " variables list (st$<varName>) or from the algebric",
  " equations list (eq$<varName> for static models) of the model '%s'.", 
  " Refactor the connection to make it valid. Connection '%s'", 
  " skipped.")

sdCoupledModelMsg$buildCoupledModel5 <- paste0(
  "sdsim::buildCoupledModel - Coupled Model '%s",
  "' warning building the connection vectors - The input '%s",
  "' unit '%s' from the model '%s",
  "' is different from the output '%s' '%s", 
  "' unit '%s",
  "' from the model '%s'. Refactor the connection '%s",
  "' if this is not wanted.")

sdCoupledModelMsg$buildCoupledModel6 <- paste0(
  "sdsim::buildCoupledModel - Coupled Model '%s'", 
  " build error: The input '%s'", 
  " is connected to more then one %s.", 
  " This input will only receive the last connection. Refactor the",
  " connections if this is not wanted. ")

#sdCoupledModel("test")$defaultScenario
sdCoupledModelMsg$defaultScenario <- paste0(
  "sdsim::defaultScenario - Coupled Model '%s'", 
  " get default scenario: The default coupled scenario must be built",
  " to retrieve it. Build it first with the method $buildCoupledModel",
  " (see help('sdCoupledModelClass')).")

# sdCoupledModel("test")$stConnections
# sdCoupledModel("test")$eqConnections
sdCoupledModelMsg$connectionsList <- paste0(
  "sdsim::connectionsList - Coupled Model '%s' %s", 
  " connection list: The default coupled scenario must be", 
  " built to retrieve the connections. Build it first with the", 
  " method $buildCoupledModel(see help('sdCoupledModelClass')).")

# sdCoupledModel("test")$indexComponents
sdCoupledModelMsg$indexComponents <- paste0(
  "sdsim::indexComponents - Coupled Model '%s'",
  " components index list: The default coupled scenario must be", 
  " built to retrieve the index list. Build it first with the", 
  " method $buildCoupledModel (see help('sdCoupledModelClass')).")


## FILE sdModel.R
## 
sdModelMsg <- list()

# sdOdeModel()
sdModelMsg$id1 <- "sdsim::id - Missing model ID: It was set to '%s'"

# sdOdeModel(1)
sdModelMsg$id2 <- paste("sdsim::id - Invalid model ID type: The model ID",
                        "must be a string. It was set to '%s'.")

# sdOdeModel("aux")
sdModelMsg$id3 <- paste0(
  "sdsim::id - Invalid model ID: The sdsim reserved word '%s' can not be used ",
  "to identify a model. The model identification was set to '%s'.")

# sdOdeModel("test", description = 3)
sdModelMsg$description <- paste("sdsim::description - Model '%s' set", 
                                "description aborted: The model description",
                                "must be a string.")

## FILE sdOdeModel.R
## 
sdOdeModelMsg <- list()

sdOdeModelMsg$initialize1 <- paste0(
  "sdsim::initialize - Model '%s' initialization:", 
  " The ode structure does not match the", 
  " help('sdOdeModel') specification. Replacement aborted.")

sdOdeModelMsg$initialize2 <- paste0(
  "sdsim::initialize - Model '%s' initialization:", 
  " The initVars structure does not match the", 
  " help('sdOdeModel') specification. Replacement aborted.")

sdOdeModelMsg$initialize3 <- paste0(
  "sdsim::initialize - Model '%s' initialization: ",
  " The postProcess structure does not match the", 
  " help('sdOdeModel') specification. Replacement aborted.")

sdOdeModelMsg$initialize4 <- paste0(
  "sdsim::initialize - Model '%s' initialization: ",
  " The trigger structure does not match specification. It", 
  " must be a data.frame, a numeric vector or a function,", 
  " see help('sdOdeModel'). If it is a character or list of",
  " characters all the elements will be evaluated or",
  " converted. Replacement aborted.")

sdOdeModelMsg$initialize5 <- paste0(
  "sdsim::initialize - Model '%s' initialization: ",
  "The event structure does not match the ", 
  "help('sdOdeModel') specification. Replacement aborted.")

sdOdeModelMsg$initialize6 <- paste0(
  "sdsim::initialize - Model '%s' Initialization warning: ", 
  "No auxiliary equations were added. %s")

sdOdeModelMsg$initialize7 <- paste0(
  "sdsim::initialize - Model '%s' Initialization: ",
  "Invalid auxiliary equations. See help('sdOdeModel') to ", 
  "learn the accepted types. Replacement aborted.")

sdOdeModelMsg$initialize8 <- paste0(
  "sdsim::initialize - Model '%s' Initialization: ",
  "Invalid global function '%s' skipped. ",
  "All the globalFunctions elements must be functions.")

sdOdeModelMsg$initialize9 <- paste0(
  "sdsim::initialize - Model '%s'", 
  " Initialization: The globalFunctions argument must be a named ",
  "list containing only functions.")

sdOdeModelMsg$initialize10 <- paste0(
  "sdsim::initialize - Ode Model '%s' initialization: The following sdsim ",
  "reserved words can not be used to name an equation and thus the ", 
  "respective auxiliary equations were skipped: %s.")

# sdOdeModel("id")$verifyModel()
sdOdeModelMsg$verifyModel0 <- paste0(
  "sdsim::verifyModel - Model '%s' ODE ",
  "verification aborted: No differential equations function was set.")

sdOdeModelMsg$verifyModel1 <- paste0(
  "sdsim::verifyModel - Model '%s'",  
  " ODE verification aborted: No valid scenario was set.")

sdOdeModelMsg$verifyModel2 <- paste0(
  "sdsim::verifyModel - Model '%s'", 
  " verification: No time sequence informed. Define the time ",
  "sequence in the default scenario. ",
  "Initial time equals 0 will be used.")

sdOdeModelMsg$verifyModel3 <- paste0(
  "sdsim::verifyModel - Model '%s'", 
  " verification: error evaluating the auxiliary equation '%s'. %s")

sdOdeModelMsg$verifyModel4 <- paste0(
  "sdsim::verifyModel - Model '%s", 
  "' verification: Evaluation of the auxiliary variable '%s", 
  "' may be incorrect. Value: %s.")

sdOdeModelMsg$verifyModel5 <- paste0(
  "sdsim::verifyModel - Model '%s", 
  "' verification: error running the ode. %s")

sdOdeModelMsg$verifyModel6 <- paste0(
  "sdsim::verifyModel - Model '%s",
  "' verification: variable $%s from the '%s",
  "' list in the $ode may be incorrect. ",
  "It has %s value.")

sdOdeModelMsg$verifyModel7 <- paste0(
  "sdsim::verifyModel - Model '%s",
  "' verification: variable $%s", 
  " in the $ode may be incorrect. It has %s", 
  " value.") 

sdOdeModelMsg$verifyModel8 <- paste0(
  "sdsim::verifyModel - Model '%s", 
  "' verification: the first element of the $ode ",
  "return value should be a numeric vector containg the ",
  "state derivatives. Wrong derivative return type: %s.")

sdOdeModelMsg$verifyModel9 <- paste0( 
  "sdsim::verifyModel - Model '%s", 
  "' verification: the number of derivatives returned by the ", 
  "$ode (%s - %s) must equal the length of the initial",
  "conditions vector (%s).")

sdOdeModelMsg$verifyModel10 <- paste0( 
  "sdsim::verifyModel - Model '%s", 
  "' verification: the $ode function should return ",
  "a list. Wrong return type: %s.")

sdOdeModelMsg$verifyModel11 <- paste0(
  "sdsim::verifyModel - Model '%s", 
  "' Ordinary Differential Equations verified.")

sdOdeModelMsg$verifyModel12 <- paste0(
  "sdsim::verifyModel - Model '%s", 
  "' verification: Scenario argument of type '%s", 
  "' discarted. It must be a valid sdScenarioClass object or ", 
  "a character string with a scenario XML or EXCEL file name.")

sdOdeModelMsg$verifyModel13 <- paste0(
  "sdsim::verifyModel - Model '%s",
  "' verification aborted: no state variables informed. Define the ", 
  "state variables in the default scenario or via the scenario argument.")

# sdOdeModelMsg$modelId1 <- function(modelId)
#   warning("sdsim::modelId - Missing model ID: It was set to '", modelId, "'.", 
#           call. = F)
# 
# sdOdeModelMsg$modelId2 <- function(modelId)
#   warning("sdsim::modelId - Invalid model ID type: The model ID must be a ",
#           "string. It was set to '", modelId, "'.", call. = F)

sdOdeModelMsg$defaultScenario <- paste0(
  "sdsim::defaultScenario - Model '%s", 
  "' set default scenario: The default scenario ",
  "must be a sdScenario object or a character string with the name of ", 
  "a scenario XML or Excel file. Create one using the ",
  "help('sdScenario') constructor. No default scenario was set.")

## FILE sdOutput.R
## 
sdOutputMsg <- list()

sdOutputMsg$plot1 <- paste0(
  "sdsim::plot - Plot output '%s': The '...' and the '%s", 
  "' arguments must have the same length. The '%s", 
  "' will not be used.")

sdOutputMsg$plot2 <- paste0(
  "sdsim::plot - Plot output '%s': the argument '...' ",
  "must be a list of character vectors containing ",
  "the formulas with the name of the variables to be plotted. See ",
  "help('sdOutput'). All the output variables will be ploted instead.")


sdOutputMsg$plot3 <- paste0(
  "sdsim::plot - Plot output '%s': ", 
  "Not all the formula variables are valid column names. ",
  "The following variables will be skipped: %s"
)

sdOutputMsg$plot4 <- paste0(
  "sdsim::plot - Plot output '%s': The x-axis variable '%s",
  "' is not valid, the simulation time sequence will be used instead.")

sdOutputMsg$summary1 <- paste0(
  "sdsim::summary - Output Trajectory Summary", sep = "\n")

sdOutputMsg$summary2 <- paste0(
  "\n\nsdsim::summary - Auxiliary Trajectory Summary", sep = "\n")

sdOutputMsg$summary3 <- paste0(
  "\n\nsdsim::summary - Time Series Trajectory Summary", sep = "\n")

## FILE sdScenario.R
## 
sdScenarioMsg <- list()

sdScenarioMsg$initialize1 <- paste0(
  "sdsim::initialize - Scenario '%s",
  "' initialization: The %s variables ", 
  "must be in a named numeric vector or list. ",
  "Scenario initialized without %s variables.")

sdScenarioMsg$initialize2 <- paste0(
  "sdsim::initialize - Scenario '%s",
  "' initialization: The %s",
  " variables must be in a named list. Scenario initialized without %s", 
  " variables.")

sdScenarioMsg$setTimeSequence <- paste0(
  "sdsim::setTimeSequence - Scenario '%s",
  "': Sequence element '%s' not set. ",
  "It must be a numeric value.")

sdScenarioMsg$setTimeSequence1 <- paste0(
  "sdsim::setTimeSequence - Scenario '%s",
  "': The end value of the time sequence must be different from the ", 
  "starting value. Argument '%s' not set.")

sdScenarioMsg$setTimeSequence2 <- paste0(
  "sdsim::setTimeSequence - Scenario '%s",
  "': The increment of the time sequence must be inside the starting ", 
  "and end values of the sequence and have the right sign (positive ", 
  "for crescent sequences and negative for descreasing ones). ", 
  "Argument '%s' not set.")

sdScenarioMsg$addInput <- paste0(
  "sdsim::addInput - Scenario '%s", 
  "' invalid interpolations: The following time series interpolation ",
  "variables are not present in the input list and will be skipped: %s")

sdScenarioMsg$id <- paste0(
  "sdsim::id - Invalid scenario ID type: The scenario ID must ",
  "be a string. It was set to '%s", "'.")

sdScenarioMsg$method1 <- paste0(
  "sdsim::method - Scenario '%s", "' set integration method ", 
  "aborted: The 'method' argument must be a string with a valid ",
  "deSolve solver name. Available solver methods: ",
  "'lsoda', 'lsode', 'lsodes', 'lsodar', 'vode', 'daspk', ",
  "'euler', 'rk4', 'ode23', 'ode45', 'radau', 'bdf', 'bdf_d', ",
  "'adams', 'impAdams', 'impAdams_d'. The default method ",
  "'lsoda' was set.")

sdScenarioMsg$method2 <- paste0(
  "sdsim::method - Scenario '%s", "' set integration method: ", 
  "The given method is not valid. Available solver methods: ",
  "'lsoda', 'lsode', 'lsodes', 'lsodar', 'vode', 'daspk', ",
  "'euler', 'rk4', 'ode23', 'ode45', 'radau', 'bdf', 'bdf_d', ",
  "'adams', 'impAdams', 'impAdams_d'. The default method ",
  "'lsoda' was set.")


sdScenarioMsg$times <- paste0(
  "sdsim::times - Scenario '%s", "' set time sequence: ",
  "The simulation time sequence must be a numeric named list ",
  "with at least one of the following three elements: 'from', ",
  "'to' and 'by', representing the initial time, the final ",
  "time and the simulation time step. ",
  "Scenario initialized without time sequence.")

sdScenarioMsg$description <- paste0(
  "sdsim::description - Scenario '%s", 
  "' set description aborted: ",
  "Description type (", '%s',
  ") not supported. It should be a named list.")

sdScenarioMsg$unit <- paste0(
  "sdsim::unit - Scenario '%s", 
  "' set unit aborted: ",
  "Unit type (", '%s',
  ") not supported. It should be a named list. ")

sdScenarioMsg$addVar1 <- paste0(
  "sdsim::addVar - Scenario '%s'" , 
  " add %s", 
  " aborted: All the scenario variables must be named.")

sdScenarioMsg$addVar2 <- paste0(
  "sdsim::addVar - Scenario '%s'", "add %s", 
  ": Unnamed variable skipped. ",
  "All the scenario variables must be named.")

sdScenarioMsg$addVar3 <- paste0(
  "sdsim::addVar - Scenario '%s' add variable: %s ",
  "values should be numeric. Variable %s",
  " will be skipped.")

sdScenarioMsg$addVar4 <- 
  "sdsim::addVar - Scenario '%s' add variable: %s of %s set to %s"

sdScenarioMsg$addVar5 <- paste0(
  "sdsim::addVar - Scenario '%s",
  "' add variable: ", "Value of %s '%s' set to %s")

sdScenarioMsg$addVar6 <- paste0(
  "sdsim::addVar - Scenario '%s", 
  "' add variable: The %s '%s", 
  "' already exists in this scenario. It will be reset to %s.")

sdScenarioMsg$addVar7 <- paste0(
  "sdsim::addVar - Scenario '%s' add variable: %s%s", 
  " will be skipped.")

sdScenarioMsg$addVar8 <- paste0(
  "sdsim::addVar - Scenario '%s' add variable: The following sdsim ",
  "reserved words can not be used to name a variable and thus the ", 
  "respective %s variables were skipped: %s."
)

sdScenarioMsg$removeVar1 <- paste0(
  "sdsim::removeVar - Scenario '%s", 
  "' remove %s",
  ": the variable name must be a string, wrong type: %s")

sdScenarioMsg$removeVar2 <- paste0(
  "sdsim::removeVar - Scenario '%s", 
  "' remove variable: %s",
  " variable %s removed")

sdScenarioMsg$removeVar3 <- paste0(
  "sdsim::removeVar - Scenario '%s",
  "' remove variable: %s",
  " variable %s not found.") 

## FILE sdStaticModel.R
##
sdStaticModelMsg <- list()

sdStaticModelMsg$initialize1 <- paste0(
  "sdsim::initialize - Static Model '%s", 
  "' initialization: ", 
  "The initVars structure does not match specification. It ", 
  "must be a function, see help('sdStaticModel'). ",
  "Replacement aborted.")

sdStaticModelMsg$initialize2 <- paste0(
  "sdsim::initialize - Static Model '%s", 
  "' initialization error: ", 
  "No algebraic equations were added. %s")

sdStaticModelMsg$initialize3 <- paste0(
  "sdsim::initialize - Static Model '%s", 
  "' initialization: Invalid algebraic equations. See ", 
  "help('sdStaticModel') to learn the accepted types. ",
  "Replacement aborted.")

sdStaticModelMsg$initialize4 <- paste0(
  "sdsim::initialize - Static Model '%s", 
  "' initialization: Invalid global function '%s' skipped. ",
  "All the globalFunctions elements must be objects of type function.")

sdStaticModelMsg$initialize5 <- paste0(
  "sdsim::initialize - Static Model '%s", 
  "' initialization: The globalFunctions argument must be a named list",
  " containing only objects of type function.")

sdStaticModelMsg$initialize6 <- paste0(
  "sdsim::initialize - Static Model '%s' initialization: The following sdsim ",
  "reserved words can not be used to name an equation and thus the ", 
  "respective algebraic equations were skipped: %s.")

sdStaticModelMsg$validate0 <- paste0(
  "sdsim::validateAlgebraicEquations Static Model '%s", 
  "' algebraic equations validation aborted: No default scenario was set.")

sdStaticModelMsg$validate1 <- paste0(
  "sdsim::validateAlgebraicEquations Static Model '%s", 
  "' validation: No time sequence informed. Define the time sequence ",
  "in the default scenario. Initial time equals 0 will be used.")

sdStaticModelMsg$validate2 <- paste0(
  "sdsim::validateAlgebraicEquations Static Model '%s", 
  "' validation: error evaluating the algebraic equation '%s'. %s")

sdStaticModelMsg$validate3 <- paste0(
  "sdsim::validateAlgebraicEquations Model '%s",
  "' validation: Evaluation of the algebraic equation '%s",
  "' may be incorrect. Value: %s.")

sdStaticModelMsg$validate4 <- paste0(
  "sdsim::validateAlgebraicEquations Static Model '%s", 
  "' Algebraic Equations Validated.")

sdStaticModelMsg$validate5 <- paste0( 
  "sdsim::validateAlgebraicEquations Model '%s", 
  "' validation: Scenario argument of type '%s", 
  "' discarted. It must be a valid sdScenarioClass object or ", 
  "a character string with a scenario XML or EXCEL file name.")

# sdStaticModelMsg$description <- paste0(
#   "sdsim::description - Static Model '%s", 
#   "' get descriptions: No default scenario was set. ",
#   "Could not get the desriptions list.")

# sdStaticModelMsg$unit <- paste0(
#   "sdsim::unit - Static Model '%s","' get units: ",
#   "No default scenario was set. Could not get the units list.")

sdStaticModelMsg$defaultscenario1 <- paste0(
  "sdsim::defaultScenario - Static Model '%s' set default scenario: ",
  "static models do not have state variables. All the state ", 
  "variables were removed before setting the default scenario.")

sdStaticModelMsg$defaultscenario2 <- paste0(
  "sdsim::defaultScenario - Static Model '%s", 
  "' set default scenario: ",
  "The default scenario must be a sdScenario object. ", 
  "Create one using the help('sdScenario') or help('sdLoadScenario')",
  "constructors. No default scenario was set.")


## FILE sdSimulator.R
##
sdSimulatorMsg <- list()

sdSimulatorMsg$sdSimulate <- paste0(
  "sdsim::sdSimulate - Simulation aborted: ",
       "A model must be informed to run the simulation.")

sdSimulatorMsg$sdSimulate2 <- paste0(
  "sdsim::sdSimulate - Simulation aborted:",
  "Invalid model class. See help('sdLoadModel') for available classes.")

sdSimulatorMsg$sdSimulateModel <- paste0(
  "sdsim::sdSimulate - Simulation of the %s model '%s' aborted: ",
  "A valid model default scenario or the scenario argument must ",
  "be informed to run the simulation.")

sdSimulatorMsg$sdSimulateAtomic0 <- paste0(
  "sdsim::sdSimulate - Simulation of the model '%s",
  "' aborted: The increment of the time sequence must be inside the ", 
  "starting and end values of the sequence and have the right sign ", 
  "(positive for crescent sequences and negative for descreasing ones). ",
  "Define a valid time sequence in the default scenario or via the ", 
  "arguments to run a simulation.")

sdSimulatorMsg$sdSimulateAtomic1 <- paste0(
  "sdsim::sdSimulate - Simulation of the model '%s",
  "' aborted: no state variables informed. Define the state variables in ", 
  "the default scenario or via the scenario argument.")

sdSimulatorMsg$sdSimulateAtomic2 <- paste0(
  "sdsim::sdSimulate - Simulation of the model '%s",
  "' aborted: No time sequence informed. Define the time sequence ",
  "in the default scenario or via the arguments.")

sdSimulatorMsg$sdSimulateAtomic3 <- paste0(
  "sdsim::sdSimulate - Simulation of the model '%s",
  "': The given method do not have root-finding capability. ",
  "The simulator will run with the default method: 'lsoda'.")

sdSimulatorMsg$sdSimulateAtomic4 <- paste0(
  "sdsim::sdSimulate - Simulation of the model '%s",
  "': The postProcess function returned the following error. %s")

sdSimulatorMsg$sdSimulateAtomic5 <- paste0(
  "sdsim::sdSimulate - Simulation of the model '%s",
  "': No method informed. ", 
  "The simulator will run with the default method: 'lsoda'.")

sdSimulatorMsg$sdSimulateAtomic6 <- paste0(
  "sdsim::sdSimulate - Simulation of the model '%s",
  "': Scenario argument of type '%s' discarted. ",
  "It must be a valid sdScenarioClass object or ", 
  "a character string with a scenario XML or EXCEL file name.")

sdSimulatorMsg$sdSimulateAtomic7 <- paste0("sdsim::sdSimulate - Simulation of ",
  "the model '%s' aborted: No differential equations function, model is empty.")

sdSimulatorMsg$sdSimulateStatic0 <- paste0(
  "sdsim::sdSimulate - Simulation of the static model '%s",
  "' aborted: No algebraic equations, model is empty.")

sdSimulatorMsg$sdSimulateStatic1 <- paste0(
  "sdsim::sdSimulate - Simulation of the static model '%s",
  "' aborted: No time sequence informed. Define the time sequence ",
  "in the default scenario or via the arguments.")

sdSimulatorMsg$sdSimulateStatic2 <- paste0(
  "sdsim::sdSimulate - Simulation of the static model '%s",
  "' aborted: The increment of the time sequence must be inside the ", 
  "starting and end values of the sequence and have the right sign ", 
  "(positive for crescent sequences and negative for descreasing ones). ",
  "Define a valid time sequence in the default scenario or via the ", 
  "arguments to run a simulation.")

sdSimulatorMsg$sdSimulateCoupled0 <- paste0(
  "sdsim::sdSimulate - Simulation of the coupled model '%s",
  "' aborted: No component differential equations function or ",
  "algebraic equation was set.")

sdSimulatorMsg$sdSimulateCoupled1 <- paste0(
  "sdsim::sdSimulate - Simulation of the coupled model '%s",
  "' aborted: Could not build the default coupled model. ",
  "Use the $buildCoupledModel method to build it.")

# sdSimulatorMsg$sdSimulateCoupled2 <- paste0(
#   "sdsim::sdSimulate - Simulation of the coupled model '%s",
#   "': The given scenario must be of type list containing multiple",
#   " sdScenario objects named with the respective model ID, ", 
#   "or of type sdScenarioClass containing a single coupled ",
#   "scenario object (see help('sdBuildCoupledScenario')). ",
#   "The given scenario of type '%s',will not be used.")

sdSimulatorMsg$sdSimulateCoupled3 <- paste0(
  "sdsim::sdSimulate - Simulation of the coupled model '%s",
  "' aborted: No time sequence informed. Define the time sequence ",
  "in the default scenario or via the arguments.")

sdSimulatorMsg$sdSimulateCoupled4 <- paste0(
  "sdsim::sdSimulate - Simulation of the coupled model '%s",
  "' aborted: no state variables informed. Define the state variables in ", 
  "the default scenario or via the 'scenario' argument.")

sdSimulatorMsg$sdSimulateCoupled5 <- paste0(
  "sdsim::sdSimulate - Simulation of the coupled model '%s",
  "': The given method do not have root-finding capability. ",
  "The simulator will run with the default method: 'lsoda'.")

sdSimulatorMsg$sdSimulateCoupled6 <- paste0(
  "sdsim::sdSimulate - Simulation of the coupled model '%s",
  "': The component '%s' postProcess function ",
  "returned the following error. %s")

sdSimulatorMsg$sdSimulateCoupled7 <- paste0(
  "sdsim::sdSimulate - Simulation of the coupled model '%s",
  "' aborted: The increment of the time sequence must be inside the ", 
  "starting and end values of the sequence and have the right sign ", 
  "(positive for crescent sequences and negative for descreasing ones). ",
  "Define a valid time sequence in the default scenario or via the ", 
  "arguments to run a simulation.")