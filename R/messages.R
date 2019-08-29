# List of warnings and errors of all the sdsim package files

## FILE: auxiliary.R
## 
auxiliaryMsg <- new.env()

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
  warning("sdsim::sdTemporalFunction - Error opening the time series file '%s'.", 
          " The default separator is ',' and the default decimal point is '.'.",
          " Temporal function transformation aborted, returned NULL. %s"
          )
)

# case use:
# sdTemporalFunction(x = TRUE)
auxiliaryMsg$sdTemporalFunction3  <- paste0( 
  warning("sdsim::sdTemporalFunction - Wrong time series variable 'x' type '%s':",
          " Please use one of the following types: fixed numeric",
          " value, matrix, data.frame or character (text file name).",
          " Transformation aborted, returned NULL.")
 )

# case use:
# sdTemporalFunction(x = 2, method = T)

auxiliaryMsg$sdTemporalFunction4  <- paste0(
  warning("sdsim::sdTemporalFunction - The given interpolation method is not", 
          " supported: See help('sdTemporalFunction') for the supported", 
          " methods. Transformation aborted, returned NULL.")
)

auxiliaryMsg$sdTemporalFunction5  <- paste0(
  warning("sdsim::sdTemporalFunction - Error interpolating the time series ", 
          "data points: Transformation aborted, returned NULL. %s")
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
constructorsMsg <- new.env()

# case use:
# sdLoadModel()
constructorsMsg$sdLoadModel1 <- function()
  stop("sdsim::sdLoadModel - Load Model aborted:",
       "\n  The Models Repository list: ",
       "\n  - ", paste(sdRepository(), collapse = "\n  - "),
       "\n\n  Use 'file = any of the above names' and 'repository = TRUE' to ",
       "load a model from the sdsim package repository.", call. = F)

# case use:
# sdLoadModel("la", repository = T)
constructorsMsg$sdLoadModel2 <- function(file)
  stop("sdsim::sdLoadModel - Load Model aborted: The given model name '", file, 
       "' is not present in the repository.",
       "\n  The Models Repository list: ",
       "\n  - ", paste(sdRepository(), collapse = "\n  - "),
       "\n\n  Use 'file = any of the above names' and 'repository = T' to ",
       "load a model from the sdsim package repository.",
       call. = F)

# case use:
# sdLoadModel("la")
constructorsMsg$sdLoadModel3 <- function(file)
  stop("sdsim::sdLoadModel - Load Model aborted: The given file '", file, 
       "' do not exists.", call. = F)

# case use:
# sdLoadModel("DESCRIPTION")
constructorsMsg$sdLoadModel4 <- function(file, e)
  stop("sdsim::sdLoadModel - Load Model '", file, 
       "': The given file is not a valid XML. Use the package functions to ", 
       "generate it. See help('sdLoadModel') and in the desired model ", 
       "help pages look for the method '$saveXml'. ", e, call. = F)

# warning when reading file
constructorsMsg$sdLoadModel5 <- function(file, w)
  warning("sdsim::sdLoadModel - Load Model from file '", file, "': ", w, call. = F)

# prefix not valid
constructorsMsg$sdLoadModel6 <- function()
  stop("sdsim::sdLoadModel - Load Model aborted: The given file is not a ", 
       "valid XML file. Generate your XML files using the sdsim package ", 
       "functions. See help('sdLoadModel') and in the desired type of model ", 
       "help pages look for the method '$saveXml'.", call. = F)

# valid xml without a model
constructorsMsg$sdLoadModel7 <- function()
  stop("sdsim::sdLoadModel - Load Model aborted: The given XML file do not ",
       "contain a model.", call. = F)

# case use:
# sdLoadScenario(file = "")
constructorsMsg$sdLoadScenario1 <- function(file)
  stop("sdsim::sdLoadScenario - Load Scenario aborted: The given file '", file, 
       "' do not exist.", call. = F)

constructorsMsg$sdLoadScenario2 <- function(file, e = NULL)
  stop("sdsim::sdLoadScenario - Load Scenario '", file, "' aborted: ",
       "The given file is not a valid XML. See help('sdLoadScenario') for ", 
       "the set of rules to encode a sdScenario in EXCEL file or use the ",
       "method '$saveXml' present in the help('sdScenarioClass') to generate",
       " a valid XML.", e, call. = F)

constructorsMsg$sdLoadScenario3 <- function(file, w)
  warning("sdsim::sdLoadScenario - Load Scenario '", file, "': ", w, call. = F)

# CASE USE
# sdLoadScenario(file = "DESCRIPTION")
constructorsMsg$sdLoadScenario4 <- function(file)
  stop("sdsim::sdLoadScenario - Load Scenario '", file, 
       "' aborted: Wrong file extension. It should be a Extensible ", 
       "Markup Language (XML) or an EXCEL file. See help('sdLoadScenario') for",
       " the set of rules to encode a sdScenario.", call. = F)

## FILE: readInputData.R
##
readInputDataMsg <- new.env()

readInputDataMsg$ConvertDataFrameToList <- paste0(
  warning("sdsim::sdLoadScenario - Load Scenario from EXCEL: an invalid ",
          "data.frame was skipped, probably with empty values. Refactor your ",
          "file if it is not wanted.")
)

readInputDataMsg$ReadDataExcel1 <- paste0(
  warning("sdsim::sdLoadScenario - Load Scenario from EXCEL: Error opening ", 
          "the file '%s'. %s")
)

readInputDataMsg$ReadDataExcel2 <- paste0(
  warning("sdsim::sdLoadScenario - Load Scenario from EXCEL: Warning opening ", 
          "the file '%s'. %s")
)

readInputDataMsg$LoadModelScenario1 <- paste0(
  warning("sdsim::sdLoadScenario - Loading scenario '%s'",
          " from EXCEL aborted: The scenario file path do not exist. ",
          "No variable will be loaded.")
)

## FILE sdCoupledModel.R
## 
sdCoupledModelMsg <- new.env()

# sdBuildCoupledScenario(scenarios = list(2,3))
sdCoupledModelMsg$sdBuildCoupledScenario1 <- function()
  stop("sdsim::sdBuildCoupledScenario - Error building the coupled scenario: ",
       "The 'scenarios' argument must be a list named with the component ID ",
       "that will use each scenario in the coupled model. ", 
       "At least one name was missing.", call. = F)

# use case
# sdBuildCoupledScenario(scenarios = list(a = 2))
sdCoupledModelMsg$sdBuildCoupledScenario2 <- function(scenComponent, modelId)
  warning("sdsim::sdBuildCoupledScenario - Building Coupled Scenario: The ", 
          "argument 'scenarios' must be a named list of not empty ", 
          "sdScenario's objects or character file names. Scenario of type '", 
          typeof(scenComponent), "' of the component '", modelId, 
          "' could not be added.", call. = F)

# sdCoupledModel("test", components = sdOdeModel("test"))
sdCoupledModelMsg$addComponent0 <- function(pCoupledModelId, id)
  warning("sdsim::addComponent - Coupled Model '", pCoupledModelId, 
          "' adding component: The component '", (id), 
          "' identification is equal to the coupled model ID. ",
          "Component skipped, refactor its ID and try to add it again.", 
          call. = F)

# sdCoupledModel("teste", components = c(sdOdeModel("test"), 
# sdStaticModel("test")))
sdCoupledModelMsg$addComponent1 <- function(pCoupledModelId, id)
  warning("sdsim::addComponent - Coupled Model '", pCoupledModelId, 
          "' adding component: The component '", (id), 
          "' already exists in the coupled model. ",
          "It will be removed and overwritten.", call. = F)

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
sdCoupledModelMsg$addConnection2 <- function(pCoupledModelId, con)
  warning("sdsim::addConnection - Coupled Model '", pCoupledModelId, 
          "' adding connection: The 5th element of the connection vector must ",
          "include the prefix st$, aux$ or eq$, indicating a state variable, ",
          "an auxiliary or an algebraic equation connection, respectively. ",
          "The connection: c(", paste(con, collapse = ","), 
          ") will be skipped.", call. = F)

# sdCoupledModel("test", connections = list(c(1,2,3,4,"aux$5"), 
#c(1,2,3,4,"aux$5")))
sdCoupledModelMsg$addConnection3 <- function(pCoupledModelId, conid)
  warning("sdsim::addConnection - Coupled Model '", pCoupledModelId, 
          "' adding connection: The connection '", (conid), 
          "' already exists in the coupled model. It will be overwritten.", 
          call. = F)

# sdCoupledModel("test", connections = c(1,2,3,4,"aux$5"))$removeConnection(2)
sdCoupledModelMsg$removeConnection <- paste0(
  "sdsim::removeConnection - Coupled Model '%s' removing connection: The ", 
  "connection ID '%s' do not exist in the coupled model and ",
  "thus can not be removed.")

# sdCoupledModel("test")$verifyModel()
sdCoupledModelMsg$verifyModel1 <- function(pCoupledModelId)
  stop("sdsim::verifyModel - Coupled Model '", pCoupledModelId, 
       "' verification error: Build the coupled model first using the method ",
       "'$buildCoupledModel'. Coupled model verification aborted.", call. = F)

sdCoupledModelMsg$verifyModel2 <- function(pCoupledModelId, typeofscenario)
  warning("sdsim::verifyModel - Coupled Model '", pCoupledModelId, 
          "' verification: The given scenario must be of type list containing ",
          "multiple sdScenario objects named with the respective component ID,",
          " or of type sdScenarioClass containing a single coupled ",
          "scenario object (see help('sdBuildCoupledScenario')). ",
          "The given scenario of type '", typeofscenario, "' will not be used.", 
          call. = F)

sdCoupledModelMsg$verifyModel3 <- function(pCoupledModelId)
  warning("sdsim::verifyModel - Coupled Model '", pCoupledModelId,
          "' verification: No time sequence informed. Define the time ",
          "sequence in the default scenario and reset it. ",
          "Initial time equal 0 will be used instead.", call. = F)

sdCoupledModelMsg$verifyModel4 <- function(pCoupledModelId, auxVar, e)
  warning("sdsim::verifyModel - Coupled Model '", pCoupledModelId,
          "' verification: Error evaluating the auxiliary equation ", 
          auxVar,". ", e, call. = F)

sdCoupledModelMsg$verifyModel5 <- function(pCoupledModelId, aux, auxVar)
  warning("sdsim::verifyModel - Coupled Model '", pCoupledModelId,
          "' verification: Evaluation of the auxiliary variable ", auxVar, 
          " may be incorrect. Value: ", utils::capture.output(aux[[auxVar]]),
          ".", call. = F)

sdCoupledModelMsg$verifyModel6 <- function(pCoupledModelId, modelId, e)
  warning("sdsim::verifyModel - Coupled Model '", pCoupledModelId, 
          "' component '", modelId, 
          "' verification: Error running the ode. ", 
          e, call. = F)

sdCoupledModelMsg$verifyModel7 <- function(pCoupledModelId, modelId, 
                                           namex, x, valuex)
  warning("sdsim::verifyModel - Coupled Model '", pCoupledModelId, 
          "' component '", modelId, "' verification: variable $", namex, 
          " from the '", x, "' list in the $componentsEquations may be ",
          "incorrect. It has '", valuex,"' value.", call. = F)

sdCoupledModelMsg$verifyModel8 <- function(pCoupledModelId, modelId, x, valuex)
  warning("sdsim::verifyModel - Coupled Model '", pCoupledModelId, 
          "' component '", modelId, "' verification: variable $", x, 
          " in the $componentsEquations may be incorrect. It has '", valuex,
          "' value.", call. = F)  

sdCoupledModelMsg$verifyModel9 <- function(pCoupledModelId, typeofdres)
  warning("sdsim::verifyModel - Coupled Model '", pCoupledModelId, 
          "' verification: the first element of the coupled model ",
          "definition return value should be a numeric vector ",
          "containg the state variables derivatives. ",
          "Wrong derivative return type: '", typeofdres, "'.", call. = F)

sdCoupledModelMsg$verifyModel10 <- function(pCoupledModelId, dRes, lenst)
  warning("sdsim::verifyModel - Coupled Model '", pCoupledModelId, 
          "' verification: the number of derivatives returned by ", 
          " $componentsEquations (", length(dRes), " - ", 
          paste0(names(dRes), collapse = ', '), 
          ") must equal the length of the initial conditions vector (", 
          lenst, ").", call. = F)

sdCoupledModelMsg$verifyModel11 <- function(pCoupledModelId, typeofdres)
  warning("sdsim::verifyModel - Coupled Model '", pCoupledModelId, 
          "' verification: the coupled model definition function should ",
          "return a list. Wrong return type: '", typeofdres,"'.", call. = F)

sdCoupledModelMsg$verifyModel12 <- function(pCoupledModelId)
  message("sdsim::verifyModel - Coupled Model '", pCoupledModelId, 
          "' Ordinary Differential Equations verified.")

sdCoupledModelMsg$buildCoupledModel0 <- paste0(
  "sdsim::buildCoupledModel - Coupled model '%s' build error: ",
  "the components are empty.")

# sdCoupledModel("test")$buildCoupledModel()
sdCoupledModelMsg$buildCoupledModel1 <- paste0(
  "sdsim::buildCoupledModel - Coupled Model '%s' build error: no component was", 
  " added. Add a component before building the coupled model.")

sdCoupledModelMsg$buildCoupledModel2 <- function(pCoupledModelId, m, id)
  warning("sdsim::buildCoupledModel - Coupled Model '", pCoupledModelId, 
          "' Error building the connection vectors: The model '", 
          m, "' is not present in the coupled components. Refactor the ",
          "connection to make it valid. Connection '", id, "' skipped.", 
          call. = F)

sdCoupledModelMsg$buildCoupledModel3 <- function(pCoupledModelId, vartype,
                                                 var, m, id)
  warning("sdsim::buildCoupledModel - Coupled Model '", pCoupledModelId, 
          "' Error Building the connection vectors: The ", vartype, " ", 
          var, " from the model '", m, "' do not exist. Refactor ",
          "the connection to make it valid. Connection '", id, 
          "' skipped.", call. = F)

sdCoupledModelMsg$buildCoupledModel4 <- function(pCoupledModelId, m, id)
  warning("sdsim::buildCoupledModel - Coupled Model '", pCoupledModelId, 
          "' Error Building the connection vectors: The ",
          "connected output can only come from the auxiliary ", 
          " equations list (aux$<varName>), from the state ",
          "variables list (st$<varName>) or from the algebric ",
          "equations list (eq$<varName> for static models) of the model '", m, 
          "'. Refactor the connection to make it valid. Connection '", id, 
          "' skipped.", call. = F)

sdCoupledModelMsg$buildCoupledModel5 <- function(pCoupledModelId, in1, u1, m1,
                                                 out2, u2, m2, id)
  warning("sdsim::buildCoupledModel - Coupled Model '", pCoupledModelId, 
          "' warning building the connection vectors - The input '",
          in1, "' unit '",  u1, "' from the model '", m1,
          "' is different from the output '", out2[[1]], "' '", 
          out2[[2]], "' unit '",  u2,
          "' from the model '", m2,"'. Refactor the connection '", id, 
          "' if this is not wanted.", call. = F)

sdCoupledModelMsg$buildCoupledModel6 <- function(pCoupledModelId, m1, in1,
                                                 vartype)
  warning("sdsim::buildCoupledModel - Coupled Model '", pCoupledModelId, 
          "' build error: The input '", paste0(m1, ".", in1), 
          "' is connected to more then one ", vartype, 
          ". This input will only receive the last connection. Refactor the ",
          "connections if this is not wanted. ", 
          call. = F)

#sdCoupledModel("test")$defaultScenario
sdCoupledModelMsg$defaultScenario <- function(pCoupledModelId)
  warning("sdsim::defaultScenario - Coupled Model '", pCoupledModelId, 
          "' get default scenario: The default coupled scenario must be built ",
          "to retrieve it. Build it first with the method $buildCoupledModel ",
          "(see help('sdCoupledModelClass')).", call. = F)

# sdCoupledModel("test")$stConnections
# sdCoupledModel("test")$eqConnections
sdCoupledModelMsg$connectionsList <- function(pCoupledModelId, vartype)
  warning("sdsim::connectionsList - Coupled Model '", pCoupledModelId, "' ", 
          vartype, " connection list: The default coupled scenario must be ", 
          "built to retrieve the connections. Build it first with the ", 
          "method $buildCoupledModel(see help('sdCoupledModelClass')).", 
          call. = F)

# sdCoupledModel("test")$indexComponents
sdCoupledModelMsg$indexComponents <- function(pCoupledModelId)
  warning("sdsim::indexComponents - Coupled Model '", pCoupledModelId,
          "' components index list: The default coupled scenario must be ", 
          "built to retrieve the index list. Build it first with the ", 
          "method $buildCoupledModel (see help('sdCoupledModelClass')).", 
          call. = F)


## FILE sdModel.R
## 
sdModelMsg <- new.env()
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
sdOdeModelMsg <- new.env()

sdOdeModelMsg$initialize1 <- function(modelId)
  warning("sdsim::initialize - Model '", modelId, "' initialization: ", 
          "The ode structure does not match the ", 
          "help('sdOdeModel') specification. Replacement aborted.", call. = F)

sdOdeModelMsg$initialize2 <- function(modelId)
  warning("sdsim::initialize - Model '", modelId, "' initialization: ", 
          "The initVars structure does not match the ", 
          "help('sdOdeModel') specification. Replacement aborted.", call. = F)

sdOdeModelMsg$initialize3 <- function(modelId)
  warning("sdsim::initialize - Model '", modelId, "' initialization: ",
          "The postProcess structure does not match the ", 
          "help('sdOdeModel') specification. Replacement aborted.", call. = F)

sdOdeModelMsg$initialize4 <- function(modelId)
  warning("sdsim::initialize - Model '", modelId, "' initialization: ",
          "The trigger structure does not match specification. It ", 
          "must be a data.frame, a numeric vector or a function, ", 
          "see help('sdOdeModel'). If it is a character or list of ",
          "characters all the elements will be evaluated or ",
          "converted. Replacement aborted.", call. = F)

sdOdeModelMsg$initialize5 <- function(modelId)
  warning("sdsim::initialize - Model '", modelId, "' initialization: ",
          "The event structure does not match the ", 
          "help('sdOdeModel') specification. Replacement aborted.", call. = F)

sdOdeModelMsg$initialize6 <- function(modelId, e)
  warning("sdsim::initialize - Model '", modelId, "' Initialization warning: ", 
          "No auxiliary equations were added. ", e, call. = F)

sdOdeModelMsg$initialize7 <- function(modelId)
  warning("sdsim::initialize - Model '", modelId, "' Initialization: ",
          "Invalid auxiliary equations. See help('sdOdeModel') to ", 
          "learn the accepted types. Replacement aborted.", call. = F)

sdOdeModelMsg$initialize8 <- function(modelId, nameGlobalFuni)
  warning("sdsim::initialize - Model '", modelId, "' Initialization: ",
          "Invalid global function '", nameGlobalFuni, "' skipped. ",
          "All the globalFunctions elements must be functions.", call. = F)

sdOdeModelMsg$initialize9 <- function(modelId)
  warning("sdsim::initialize - Model '", modelId, 
          "' Initialization: The globalFunctions argument must be a named ",
          "list containing only functions.", call. = F)

sdOdeModelMsg$initialize10 <- paste0(
  "sdsim::initialize - Ode Model '%s' initialization: The following sdsim ",
  "reserved words can not be used to name an equation and thus the ", 
  "respective auxiliary equations were skipped: %s.")

# sdOdeModel("id")$verifyModel()
sdOdeModelMsg$verifyModel0 <- paste0("sdsim::verifyModel - Model '%s' ODE ",
       "verification aborted: No differential equations function was set.")

sdOdeModelMsg$verifyModel1 <- function(pModelId)
  stop("sdsim::verifyModel - Model '", pModelId, 
       "' ODE verification aborted: No valid scenario was set.", call. = F)

sdOdeModelMsg$verifyModel2 <- function(pModelId)
  warning("sdsim::verifyModel - Model '", pModelId, 
          "' verification: No time sequence informed. Define the time ",
          "sequence in the default scenario. ",
          "Initial time equals 0 will be used.", call. = F)

sdOdeModelMsg$verifyModel3 <- function(pModelId, auxVar, e)
  warning("sdsim::verifyModel - Model '", pModelId, 
          "' verification: error evaluating the auxiliary equation '", 
          auxVar, "'. ", e, call. = F)

sdOdeModelMsg$verifyModel4 <- function(pModelId, auxVar, auxValue)
  warning("sdsim::verifyModel - Model '", pModelId, 
          "' verification: Evaluation of the auxiliary variable '", auxVar, 
          "' may be incorrect. Value: ", auxValue, ".", call. = F)

sdOdeModelMsg$verifyModel5 <- function(pModelId, e)
  warning("sdsim::verifyModel - Model '", pModelId, 
          "' verification: error running the ode. ", e, 
          call. = F)

sdOdeModelMsg$verifyModel6 <- function(pModelId, xname, x, xvalue)
  warning("sdsim::verifyModel - Model '", pModelId, 
          "' verification: variable $", xname, " from the '", x,
          "' list in the $ode may be incorrect. ",
          "It has ", xvalue, " value.", call. = F)

sdOdeModelMsg$verifyModel7 <- function(pModelId, x, xvalue)
  warning("sdsim::verifyModel - Model '", pModelId, 
          "' verification: variable $", x, 
          " in the $ode may be incorrect. It has ", xvalue, 
          " value.", call. = F) 

sdOdeModelMsg$verifyModel8 <- function(pModelId, typeofres) 
  warning("sdsim::verifyModel - Model '", pModelId, 
          "' verification: the first element of the $ode ",
          "return value should be a numeric vector containg the ",
          "state derivatives. Wrong derivative return type: ", typeofres, ".", 
          call. = F)

sdOdeModelMsg$verifyModel9 <- function(pModelId, dRes, lenst) 
  warning("sdsim::verifyModel - Model '", pModelId, 
          "' verification: the number of derivatives returned by the ", 
          "$ode (", length(dRes), " - ", 
          paste0(names(dRes), collapse = ', '), 
          ") must equal the length of the initial conditions vector (", 
          lenst, ").", call. = F)

sdOdeModelMsg$verifyModel10 <- function(pModelId, typeofres) 
  warning("sdsim::verifyModel - Model '", pModelId, 
          "' verification: the $ode function should return ",
          "a list. Wrong return type: ", typeofres,".", call. = F)

sdOdeModelMsg$verifyModel11 <- function(pModelId) 
  message("sdsim::verifyModel - Model '", pModelId, 
          "' Ordinary Differential Equations verified.")

sdOdeModelMsg$verifyModel12 <- function(pModelId, typeofscen) 
  warning("sdsim::verifyModel - Model '", pModelId, 
          "' verification: Scenario argument of type '", typeofscen, 
          "' discarted. It must be a valid sdScenarioClass object or ", 
          "a character string with a scenario XML or EXCEL file name.", 
          call. = F)

sdOdeModelMsg$verifyModel13 <- function(modelId)
  stop("sdsim::verifyModel - Model '", modelId,
       "' verification aborted: no state variables informed. Define the ", 
       "state variables in the default scenario or via the scenario argument.", 
       call. = F)

# sdOdeModelMsg$modelId1 <- function(modelId)
#   warning("sdsim::modelId - Missing model ID: It was set to '", modelId, "'.", 
#           call. = F)
# 
# sdOdeModelMsg$modelId2 <- function(modelId)
#   warning("sdsim::modelId - Invalid model ID type: The model ID must be a ",
#           "string. It was set to '", modelId, "'.", call. = F)

sdOdeModelMsg$defaultScenario <- function(pModelId)
  warning("sdsim::defaultScenario - Model '", pModelId, 
          "' set default scenario: The default scenario ",
          "must be a sdScenario object or a character string with the name of ", 
          "a scenario XML or Excel file. Create one using the ",
          "help('sdScenario') constructor. No default scenario was set.", 
          call. = F)

## FILE sdOutput.R
## 
sdOutputMsg <- new.env()

sdOutputMsg$plot1 <- function(pOutputId, plotarg)
  warning("sdsim::plot - Plot output '", pOutputId, "': The '...' and the '", 
          plotarg, "' arguments must have the same length. The '", plotarg, 
          "' will not be used.", call. = F)

sdOutputMsg$plot2 <- function(pOutputId)
  warning("sdsim::plot - Plot output '", pOutputId, "': the argument '...' ",
          "must be a list of character vectors containing ",
          "the formulas with the name of the variables to be plotted. See ",
          "help('sdOutput'). All the output variables will be ploted instead.",
          call. = F)

sdOutputMsg$plot3 <- function(pOutputId, yaxisArray, namesData)
  warning("sdsim::plot - Plot output '", pOutputId, 
          "': Not all the formula variables are valid column names. ",
          "The following variables will be skipped: ", 
          paste(yaxisArray[!(yaxisArray %in% namesData)], 
                collapse = ", "), call. = F)

sdOutputMsg$plot4 <- function(pOutputId, xaxis)
  warning("sdsim::plot - Plot output '", pOutputId, "': The x-axis variable '", 
          xaxis, 
          "' is not valid, the simulation time sequence will be used instead.",
          call. = F)

sdOutputMsg$summary1 <- function()
  message("sdsim::summary - Output Trajectory Summary", sep = "\n")

sdOutputMsg$summary2 <- function()
  message("\n\nsdsim::summary - Auxiliary Trajectory Summary", sep = "\n")

sdOutputMsg$summary3 <- function()
  message("\n\nsdsim::summary - Time Series Trajectory Summary", sep = "\n")

## FILE sdScenario.R
## 
sdScenarioMsg <- new.env()

sdScenarioMsg$initialize1 <- function(scenarioId, varType)
  warning("sdsim::initialize - Scenario '", scenarioId, 
          "' initialization: The ", varType, " variables ", 
          "must be in a named numeric vector or list. ",
          "Scenario initialized without ", varType, " variables.", call. = F)

sdScenarioMsg$initialize2 <- function(scenarioId, varType)
  warning("sdsim::initialize - Scenario '", scenarioId, 
          "' initialization: The ", varType,
          " variables must be in a named list. Scenario initialized without ", 
          varType, " variables.", call. = F)

sdScenarioMsg$setTimeSequence <- function(pScenarioId, varType)
  warning("sdsim::setTimeSequence - Scenario '", pScenarioId, 
          "': Sequence element '", varType, "' not set. ",
          "It must be a numeric value.", call. = F)

sdScenarioMsg$setTimeSequence1 <- function(pScenarioId, varType)
  warning("sdsim::setTimeSequence - Scenario '", pScenarioId, 
          "': The end value of the time sequence must be different from the ", 
          "starting value. Argument '", varType, "' not set.", call. = F)

sdScenarioMsg$setTimeSequence2 <- function(pScenarioId, varType)
  warning("sdsim::setTimeSequence - Scenario '", pScenarioId, 
          "': The increment of the time sequence must be inside the starting ", 
          "and end values of the sequence and have the right sign (positive ", 
          "for crescent sequences and negative for descreasing ones). ", 
          "Argument '", varType, "' not set.", call. = F)

sdScenarioMsg$addInput <- function(pScenarioId, namesInterpol, namesInput)
  warning("sdsim::addInput - Scenario '", pScenarioId, 
          "' invalid interpolations: The following time series interpolation ",
          "variables are not present in the input list and will be skipped: ",
          capture.output(namesInterpol[!(namesInterpol %in% namesInput)]),
          call. = F)

sdScenarioMsg$id <- function(scenarioId)
  warning("sdsim::id - Invalid scenario ID type: The scenario ID must ",
          "be a string. It was set to '", scenarioId, "'.", call. = F)

sdScenarioMsg$method1 <- function(pScenarioId)
  warning("sdsim::method - Scenario '", pScenarioId,"' set integration method ", 
          "aborted: The 'method' argument must be a string with a valid ",
          "deSolve solver name. Available solver methods: ",
          "'lsoda', 'lsode', 'lsodes', 'lsodar', 'vode', 'daspk', ",
          "'euler', 'rk4', 'ode23', 'ode45', 'radau', 'bdf', 'bdf_d', ",
          "'adams', 'impAdams', 'impAdams_d'. The default method ",
          "'lsoda' was set.", call. = F)

sdScenarioMsg$method2 <- function(pScenarioId)
  warning("sdsim::method - Scenario '",pScenarioId,"' set integration method: ", 
          "The given method is not valid. Available solver methods: ",
          "'lsoda', 'lsode', 'lsodes', 'lsodar', 'vode', 'daspk', ",
          "'euler', 'rk4', 'ode23', 'ode45', 'radau', 'bdf', 'bdf_d', ",
          "'adams', 'impAdams', 'impAdams_d'. The default method ",
          "'lsoda' was set.", call. = F)


sdScenarioMsg$times <- function(pScenarioId)
  warning("sdsim::times - Scenario '", pScenarioId, "' set time sequence: ",
          "The simulation time sequence must be a numeric named list ",
          "with at least one of the following three elements: 'from', ",
          "'to' and 'by', representing the initial time, the final ",
          "time and the simulation time step. ",
          "Scenario initialized without time sequence.", call. = F)

sdScenarioMsg$description <- function(pScenarioId, typeofdescription)
  warning("sdsim::description - Scenario '", pScenarioId, 
          "' set description aborted: ",
          "Description type (", typeofdescription,
          ") not supported. It should be a named list.", call. = F)

sdScenarioMsg$unit <- function(pScenarioId, typeofunit)
  warning("sdsim::unit - Scenario '", pScenarioId, 
          "' set unit aborted: ",
          "Unit type (", typeofunit,
          ") not supported. It should be a named list. ", call. = F)

sdScenarioMsg$addVar1 <- function(pScenarioId, varType)
  warning("sdsim::addVar - Scenario '", pScenarioId, "' add ", 
          varType, " aborted: All the scenario variables must be named.",
          call. = F)

sdScenarioMsg$addVar2 <- function(pScenarioId, varType)
  warning("sdsim::addVar - Scenario '", pScenarioId, "' add ", 
          varType, ": Unnamed variable skipped. ",
          "All the scenario variables must be named.", call. = F)

sdScenarioMsg$addVar3 <- function(pScenarioId, varType, var)
  warning("sdsim::addVar - Scenario '", pScenarioId, "' add variable: ",
          paste(gsub("(^.)", "\\U\\1", varType, perl = T), 
                "values should be numeric. Variable "), var, 
          " will be skipped.", call. = F)

sdScenarioMsg$addVar4 <- function(pScenarioId, varType, var, varValue)
  message("sdsim::addVar - Scenario '", pScenarioId, 
          "' add variable: ",
          gsub("(^.)", "\\U\\1", varType, perl = T), " of ", var, " set to ", 
          capture.output(varValue))

sdScenarioMsg$addVar5 <- function(pScenarioId, varType, var, varValue)
  message("sdsim::addVar - Scenario '", pScenarioId, 
          "' add variable: ", "Value of ", varType, " '", var, "' set to ", 
          capture.output(varValue))

sdScenarioMsg$addVar6 <- function(pScenarioId, varType, var, varValue)
  warning("sdsim::addVar - Scenario '", pScenarioId, 
          "' add variable: The ", varType, " '", var, 
          "' already exists in this scenario. It will be reset to ", 
          capture.output(varValue), ".", call. = F)

sdScenarioMsg$addVar7 <- function(pScenarioId, varType, var)
  warning("sdsim::addVar - Scenario '", pScenarioId, 
          "' add variable: ", paste(gsub("(^.)", "\\U\\1", varType, perl = T), 
                                    "values should not be null. Variable "), 
          var, " will be skipped.", call. = F)

sdScenarioMsg$addVar8 <- paste0(
  "sdsim::addVar - Scenario '%s' add variable: The following sdsim ",
  "reserved words can not be used to name a variable and thus the ", 
  "respective %s variables were skipped: %s."
)

sdScenarioMsg$removeVar1 <- function(pScenarioId, varType, typeofx)
  warning("sdsim::removeVar - Scenario '", pScenarioId, 
          "' remove ", varType,
          ": the variable name must be a string, wrong type: ", typeofx, 
          call. = F)

sdScenarioMsg$removeVar2 <- function(pScenarioId, varType, var)
  message("sdsim::removeVar - Scenario '", pScenarioId, 
          "' remove variable: ", gsub("(^.)", "\\U\\1", varType, perl = T), 
          " variable ", var, " removed")

sdScenarioMsg$removeVar3 <- function(pScenarioId, varType, var)
  warning("sdsim::removeVar - Scenario '", pScenarioId, 
          "' remove variable: ", gsub("(^.)", "\\U\\1", varType, perl = T), 
          " variable ", var, " not found.", call. = F) 

## FILE sdStaticModel.R
##
sdStaticModelMsg <- new.env()

sdStaticModelMsg$initialize1 <- function(staticModelId)
  warning("sdsim::initialize - Static Model '", staticModelId, 
          "' initialization: ", 
          "The initVars structure does not match specification. It ", 
          "must be a function, see help('sdStaticModel'). ",
          "Replacement aborted.", call. = F)

sdStaticModelMsg$initialize2 <- function(staticModelId, e)
  warning("sdsim::initialize - Static Model '", staticModelId, 
          "' initialization error: ", 
          "No algebraic equations were added. ", e, call. = F)

sdStaticModelMsg$initialize3 <- function(staticModelId)
  warning("sdsim::initialize - Static Model '", staticModelId, 
          "' initialization: Invalid algebraic equations. See ", 
          "help('sdStaticModel') to learn the accepted types. ",
          "Replacement aborted.", call. = F)

sdStaticModelMsg$initialize4 <- function(staticModelId, nameglobalfuni)
  warning("sdsim::initialize - Static Model '", staticModelId, 
          "' initialization: Invalid global function '", 
          nameglobalfuni, "' skipped. ",
          "All the globalFunctions elements must be objects of type function.", 
          call. = F)

sdStaticModelMsg$initialize5 <- function(staticModelId)
  warning("sdsim::initialize - Static Model '", staticModelId, 
          "' initialization: The globalFunctions argument must be a named list",
          " containing only objects of type function.", call. = F)

sdStaticModelMsg$initialize6 <- paste0(
  "sdsim::initialize - Static Model '%s' initialization: The following sdsim ",
  "reserved words can not be used to name an equation and thus the ", 
  "respective algebraic equations were skipped: %s."
)

sdStaticModelMsg$validate0 <- function(pstaticModelId)
  stop("sdsim::validateAlgebraicEquations Static Model '", pstaticModelId, 
       "' algebraic equations validation aborted: No default scenario was set.", 
       call. = F)

sdStaticModelMsg$validate1 <- function(pstaticModelId)
  warning("sdsim::validateAlgebraicEquations Static Model '", pstaticModelId, 
          "' validation: No time sequence informed. Define the time sequence ",
          "in the default scenario. Initial time equals 0 will be used.", 
          call. = F)

sdStaticModelMsg$validate2 <- function(pstaticModelId, equationsVar, e)
  warning("sdsim::validateAlgebraicEquations Static Model '", pstaticModelId, 
          "' validation: error evaluating the algebraic equation '", 
          equationsVar, "'. ", e,  call. = F)

sdStaticModelMsg$validate3 <- function(pstaticModelId, equationsVar, eqValue)
  warning("sdsim::validateAlgebraicEquations Model '", pstaticModelId, 
          "' validation: Evaluation of the algebraic equation '", equationsVar, 
          "' may be incorrect. Value: ", capture.output(eqValue), ".",call. = F)

sdStaticModelMsg$validate4 <- function(pstaticModelId)
  message("sdsim::validateAlgebraicEquations Static Model '", pstaticModelId, 
          "' Algebraic Equations Validated.")

sdStaticModelMsg$validate5 <- function(pstaticModelId, typeofscen) 
  warning("sdsim::validateAlgebraicEquations Model '", pstaticModelId, 
          "' validation: Scenario argument of type '", typeofscen, 
          "' discarted. It must be a valid sdScenarioClass object or ", 
          "a character string with a scenario XML or EXCEL file name.", 
          call. = F)

sdStaticModelMsg$description <- function(pstaticModelId)
  warning("sdsim::description - Static Model '", pstaticModelId, 
          "' get descriptions: No default scenario was set. ",
          "Could not get the desriptions list.", call. = F)

sdStaticModelMsg$unit <- function(pstaticModelId)
  warning("sdsim::unit - Static Model '", pstaticModelId, "' get units: ",
          "No default scenario was set. Could not get the units list.",
          call. = F)

sdStaticModelMsg$defaultscenario1 <- paste0(
  "sdsim::defaultScenario - Static Model '%s' set default scenario: ",
  "static models do not have state variables. All the state ", 
  "variables were removed before setting the default scenario.")

sdStaticModelMsg$defaultscenario2 <- function(pstaticModelId)
  warning("sdsim::defaultScenario - Static Model '", pstaticModelId, 
          "' set default scenario: ",
          "The default scenario must be a sdScenario object. ", 
          "Create one using the help('sdScenario') or help('sdLoadScenario')",
          "constructors. No default scenario was set.", call. = F)

## FILE sdSimulator.R
##
sdSimulatorMsg <- new.env()

sdSimulatorMsg$sdSimulate <- paste("sdsim::sdSimulate - Simulation aborted:",
       "Invalid model class. See help('sdLoadModel') for available classes.")

sdSimulatorMsg$sdSimulate <- function()
  stop("sdsim::sdSimulate - Simulation aborted: ",
       "A model must be informed to run the simulation.", call. = F)

sdSimulatorMsg$sdSimulateModel <- paste0(
  "sdsim::sdSimulate - Simulation of the %s model '%s' aborted: ",
  "A valid model default scenario or the scenario argument must ",
  "be informed to run the simulation.")

sdSimulatorMsg$sdSimulateAtomic0 <- function(modelId)
  stop("sdsim::sdSimulate - Simulation of the model '", modelId,
       "' aborted: The increment of the time sequence must be inside the ", 
       "starting and end values of the sequence and have the right sign ", 
       "(positive for crescent sequences and negative for descreasing ones). ",
       "Define a valid time sequence in the default scenario or via the ", 
       "arguments to run a simulation.", call. = F)

sdSimulatorMsg$sdSimulateAtomic1 <- function(modelId)
  stop("sdsim::sdSimulate - Simulation of the model '", modelId,
       "' aborted: no state variables informed. Define the state variables in ", 
       "the default scenario or via the scenario argument.", call. = F)

sdSimulatorMsg$sdSimulateAtomic2 <- function(modelId)
  stop("sdsim::sdSimulate - Simulation of the model '", modelId,
       "' aborted: No time sequence informed. Define the time sequence ",
       "in the default scenario or via the arguments.", call. = F)

sdSimulatorMsg$sdSimulateAtomic3 <- function(modelId)
  warning("sdsim::sdSimulate - Simulation of the model '", modelId,
          "': The given method do not have root-finding capability. ",
          "The simulator will run with the default method: 'lsoda'.", call. = F)

sdSimulatorMsg$sdSimulateAtomic4 <- function(modelId, e)
  warning("sdsim::sdSimulate - Simulation of the model '", modelId,
          "': The postProcess function returned the following error. ", 
          e, call. = F)

sdSimulatorMsg$sdSimulateAtomic5 <- function(modelId)
  warning("sdsim::sdSimulate - Simulation of the model '", modelId,
       "': No method informed. ", 
       "The simulator will run with the default method: 'lsoda'.", call. = F)

sdSimulatorMsg$sdSimulateAtomic6 <- function(modelId, typeofscen)
  warning("sdsim::sdSimulate - Simulation of the model '", modelId,
          "': Scenario argument of type '", typeofscen, "' discarted. ",
          "It must be a valid sdScenarioClass object or ", 
          "a character string with a scenario XML or EXCEL file name.", 
          call. = F)

sdSimulatorMsg$sdSimulateAtomic7 <- paste0("sdsim::sdSimulate - Simulation of ",
  "the model '%s' aborted: No differential equations function, model is empty.")

sdSimulatorMsg$sdSimulateStatic0 <- function(staticModelId)
  stop("sdsim::sdSimulate - Simulation of the static model '", staticModelId,
       "' aborted: No algebraic equations, model is empty.", call. = F)

sdSimulatorMsg$sdSimulateStatic1 <- function(staticModelId)
  stop("sdsim::sdSimulate - Simulation of the static model '", staticModelId,
       "' aborted: No time sequence informed. Define the time sequence ",
       "in the default scenario or via the arguments.", call. = F)

sdSimulatorMsg$sdSimulateStatic2 <- function(staticModelId)
  stop("sdsim::sdSimulate - Simulation of the static model '", staticModelId,
       "' aborted: The increment of the time sequence must be inside the ", 
       "starting and end values of the sequence and have the right sign ", 
       "(positive for crescent sequences and negative for descreasing ones). ",
       "Define a valid time sequence in the default scenario or via the ", 
       "arguments to run a simulation.", call. = F)

sdSimulatorMsg$sdSimulateCoupled0 <- function(coupledModelId)
  stop("sdsim::sdSimulate - Simulation of the coupled model '", coupledModelId,
       "' aborted: No component differential equations function or ",
       "algebraic equation was set.", 
       call. = F)

sdSimulatorMsg$sdSimulateCoupled1 <- function(coupledModelId)
  stop("sdsim::sdSimulate - Simulation of the coupled model '", coupledModelId,
       "' aborted: Could not build the default coupled model. ",
       "Use the $buildCoupledModel method to build it.", call. = F)

sdSimulatorMsg$sdSimulateCoupled2 <- function(coupledModelId, typeofscenario)
  warning("sdsim::sdSimulate - Simulation of the coupled model '", 
          coupledModelId,
          "': The given scenario must be of type list containing multiple",
          " sdScenario objects named with the respective model ID, ", 
          "or of type sdScenarioClass containing a single coupled ",
          "scenario object (see help('sdBuildCoupledScenario')). ",
          "The given scenario of type '", typeofscenario, "' will not be used.", 
          call. = F)

sdSimulatorMsg$sdSimulateCoupled3 <- function(coupledModelId)
  stop("sdsim::sdSimulate - Simulation of the coupled model '", coupledModelId,
       "' aborted: No time sequence informed. Define the time sequence ",
       "in the default scenario or via the arguments.", call. = F)

sdSimulatorMsg$sdSimulateCoupled4 <- function(coupledModelId)
  stop("sdsim::sdSimulate - Simulation of the coupled model '", coupledModelId,
       "' aborted: no state variables informed. Define the state variables in ", 
       "the default scenario or via the 'scenario' argument.", call. = F)

sdSimulatorMsg$sdSimulateCoupled5 <- function(coupledModelId)
  warning("sdsim::sdSimulate - Simulation of the coupled model '", 
          coupledModelId,
          "': The given method do not have root-finding capability. ",
          "The simulator will run with the default method: 'lsoda'.",
          call. = F)

sdSimulatorMsg$sdSimulateCoupled6 <- function(coupledModelId, modelId)
  warning("sdsim::sdSimulate - Simulation of the coupled model '", 
          coupledModelId,
          "': The component '", modelId, "' postProcess function ",
          "returned the following error. ", e, call. = F)

sdSimulatorMsg$sdSimulateCoupled7 <- function(coupledModelId)
  stop("sdsim::sdSimulate - Simulation of the coupled model '", coupledModelId,
       "' aborted: The increment of the time sequence must be inside the ", 
       "starting and end values of the sequence and have the right sign ", 
       "(positive for crescent sequences and negative for descreasing ones). ",
       "Define a valid time sequence in the default scenario or via the ", 
       "arguments to run a simulation.", call. = F)