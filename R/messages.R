# List of warnings and errors of all the sdsim package files

## FILE: auxiliary.R
## 
auxiliaryMsg <- new.env()

# use case:
# sdInitEquations(list(2,3))
auxiliaryMsg$sdInitEq <- function(eqName, eq)
  warning("sdsim::sdInitEquations - Initializing equations: The ", eqName, 
          " equations argument must be a ",
          "named list of R-expressions and/or strings with equations in ",
          "R-format. The element of type ", typeof(eq), 
          " will be skipped.", call. = F)

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

auxiliaryMsg$sdTemporalFunctionList <- function() 
  warning("sdsim::sdTemporalFunctionList - Error Transforming the time series ",
          "variables: The size of the list 'x' must match the size of the list ",
          "'methods'. Transformation aborted, returned NULL.", call. = F)

# case use:
# sdTemporalFunction(x = "lala")
auxiliaryMsg$sdTemporalFunction1  <- function(timeSeriesDirectory, x) 
  warning("sdsim::sdTemporalFunction - Error: The time series file '", 
          paste0(timeSeriesDirectory, x), "' do not exist.",
          " Temporal function transformation aborted, returned NULL.", 
          call. = F)

# case use:
# sdTemporalFunction(x = "DESCRIPTION")
auxiliaryMsg$sdTemporalFunction2  <- function(timeSeriesDirectory, x, e) 
  warning("sdsim::sdTemporalFunction - Error opening the time series file '", 
          paste0(timeSeriesDirectory, x), "'. The default separator is ", 
          "',' and the default decimal point is '.'. ",
          "Temporal function transformation aborted, returned NULL. ", 
          e, call. = F)

# case use:
# sdTemporalFunction(x = TRUE)
auxiliaryMsg$sdTemporalFunction3  <- function(x) 
  warning("sdsim::sdTemporalFunction - Wrong time series variable 'x' type '",
          typeof(x), "': Please use one of the following types: fixed numeric ",
          "value, matrix, data.frame or character (text file name). ",
          "Transformation aborted, returned NULL.", call. = F)

# case use:
# sdTemporalFunction(x = 2, method = T)
auxiliaryMsg$sdTemporalFunction4  <- function() 
  warning("sdsim::sdTemporalFunction - The given interpolation method is not ", 
          "supported: See help('sdTemporalFunction') for the supported ", 
          "methods. Transformation aborted, returned NULL.", call. = F)

# case use:
# bb <- sdLoadModel("BouncingBall", repository = T)
# sdSimulate(bb, scenario = sdScenario("", parameter = c(a = 2)))
auxiliaryMsg$MergeLists <- function(x, listName)
  warning("sdsim::MergeLists - The variable '", x, "' from the scenario ", 
          listName, " list does not exist in the model's default scenario.", 
          call. = F)

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
       "help pages look for the method '$saveToXml'. ", e, call. = F)

# warning when reading file
constructorsMsg$sdLoadModel5 <- function(file, w)
  warning("sdsim::sdLoadModel - Load Model from file '", file, "': ", w, call. = F)

# prefix not valid
constructorsMsg$sdLoadModel6 <- function()
  stop("sdsim::sdLoadModel - Load Model aborted: The given file is not a ", 
       "valid XML file. Generate your XML files using the sdsim package ", 
       "functions. See help('sdLoadModel') and in the desired type of model ", 
       "help pages look for the method '$saveToXml'.", call. = F)

constructorsMsg$sdLoadModel7 <- function()
  stop("sdsim::sdLoadModel - Load Model aborted: The given XML file do not ",
       "contain a model.", call. = F)

# case use:
# sdLoadScenario(file = "")
constructorsMsg$sdLoadScenario1 <- function(file)
  stop("sdsim::sdLoadScenario - Load Scenario aborted: The given file '", file, 
       "' do not exists.", call. = F)

constructorsMsg$sdLoadScenario2 <- function(file, e)
  stop("sdsim::sdLoadScenario - Load Scenario '", file, "' aborted: ",
       "The given file is not a valid XML. See help('sdLoadScenario') for ", 
       "the set of rules to encode a sdScenario in EXCEL file or use the ",
       "method '$saveToXml' present in the help('sdScenarioClass') to generate",
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

readInputDataMsg$ConvertDataFrameToList <- function()
  warning("sdsim::sdLoadScenario - Load Scenario from EXCEL: an invalid ",
          "data.frame was skipped, probably with empty values. Refactor your ",
          "file if it is not wanted.", call. = F)

readInputDataMsg$ReadDataExcel1 <- function(fileName, e)
  warning("sdsim::sdLoadScenario - Load Scenario from EXCEL: Error opening ", 
          "the file '", fileName, "'. ", e, call. = F)

readInputDataMsg$ReadDataExcel2 <- function(fileName, w)
  warning("sdsim::sdLoadScenario - Load Scenario from EXCEL: Warning opening ", 
          "the file '", fileName, "'. ", w,  call. = F)

readInputDataMsg$LoadModelScenario1 <- function(file)
  warning("sdsim::sdLoadScenario - Loading scenario '", file, 
          "' from EXCEL aborted: The scenario file path do not exist. ",
          "No variable will be loaded.", call. = F)

## FILE sdCoupledModel.R
## 
sdCoupledModelMsg <- new.env()

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

sdCoupledModelMsg$addComponent0 <- function(pcoupledModelId, id)
  warning("sdsim::addComponent - Coupled Model '", pcoupledModelId, 
          "' adding component: The component '", (id), 
          "' identification is equal to the coupled model ID. ",
          "Component skipped, refactor its ID and try to add it again.", 
          call. = F)

sdCoupledModelMsg$addComponent1 <- function(pcoupledModelId, id)
  warning("sdsim::addComponent - Coupled Model '", pcoupledModelId, 
          "' adding component: The component '", (id), 
          "' already exists in the coupled model. ",
          "It will be removed and overwritten.", call. = F)

sdCoupledModelMsg$addComponent2 <- function(pcoupledModelId, typeofmodel)
  warning("sdsim::addComponent - Coupled Model '", pcoupledModelId, 
          "' adding component: the argument '...' must be a list of not empty ",
          "sdModel's, sdStaticModel's, sdCoupledModel's or character XML file ",
          "names. Component of type ", typeofmodel, " could not be added.", 
          call. = F)

sdCoupledModelMsg$addConnection1 <- function(pcoupledModelId, con)
  warning("sdsim::addConnection - Coupled Model '", pcoupledModelId, 
          "' adding connection: Each connection must be a vector with 5 ", 
          "elements. The connection: c(", paste(con, collapse = ","), 
          ") will be skipped.", call. = F)

sdCoupledModelMsg$addConnection2 <- function(pcoupledModelId, con)
  warning("sdsim::addConnection - Coupled Model '", pcoupledModelId, 
          "' adding connection: The 5th element of the connection vector must ",
          "include the prefix st$, aux$ or eq$, indicating a state variable, ",
          "an auxiliary or an algebraic equation connection, respectively.",
          "The connection: c(", paste(con, collapse = ","), 
          ") will be skipped.", call. = F)

sdCoupledModelMsg$addConnection3 <- function(pcoupledModelId, conid)
  warning("sdsim::addConnection - Coupled Model '", pcoupledModelId, 
          "' adding connection: The connection '", (conid), 
          "' already exists in the coupled model. It will be overwritten.", 
          call. = F)

sdCoupledModelMsg$verifyModel1 <- function(pcoupledModelId)
  stop("sdsim::verifyModel - Coupled Model '", pcoupledModelId, 
       "' validation error: Build the coupled model first using the method ",
       "'$buildCoupledModel'. Coupled model validation aborted.", call. = F)

sdCoupledModelMsg$verifyModel2 <- function(pcoupledModelId, typeofscenario)
  warning("sdsim::verifyModel - Coupled Model '", pcoupledModelId, 
          "' validation: The given scenario must be of type list containing ",
          "multiple sdScenario objects named with the respective component ID,",
          " or of type sdScenarioClass containing a single coupled ",
          "scenario object (see help('sdBuildCoupledScenario')). ",
          "The given scenario of type '", typeofscenario, "' will not be used.", 
          call. = F)

sdCoupledModelMsg$verifyModel3 <- function(pcoupledModelId)
  warning("sdsim::verifyModel - Coupled Model '", pcoupledModelId,
          "' validation: No time sequence informed. Define the time sequence ",
          "in the default scenario and reset it. ",
          "Initial time equal 0 will be used instead.", call. = F)

sdCoupledModelMsg$verifyModel4 <- function(pcoupledModelId, auxVar, e)
  warning("sdsim::verifyModel - Coupled Model '", pcoupledModelId,
          "' validation: Error evaluating the auxiliary equation ", 
          auxVar,". ", e, call. = F)

sdCoupledModelMsg$verifyModel5 <- function(pcoupledModelId, aux, auxVar)
  warning("sdsim::verifyModel - Coupled Model '", pcoupledModelId,
          "' validation: Evaluation of the auxiliary variable ", auxVar, 
          " may be incorrect. Value: ", utils::capture.output(aux[[auxVar]]),
          ".", call. = F)

sdCoupledModelMsg$verifyModel6 <- function(pcoupledModelId, modelId, e)
  warning("sdsim::verifyModel - Coupled Model '", pcoupledModelId, 
          "' component '", modelId, 
          "' validation: Error running the DifferentialEquations. ", 
          e, call. = F)

sdCoupledModelMsg$verifyModel7 <- function(pcoupledModelId, modelId, 
                                           namex, x, valuex)
  warning("sdsim::verifyModel - Coupled Model '", pcoupledModelId, 
          "' component '", modelId, "' validation: variable $", namex, 
          " from the '", x, "' list in the $componentsEquations may be ",
          "incorrect. It has '", valuex,"' value.", call. = F)

sdCoupledModelMsg$verifyModel8 <- function(pcoupledModelId, modelId, x, valuex)
  warning("sdsim::verifyModel - Coupled Model '", pcoupledModelId, 
          "' component '", modelId, "' validation: variable $", x, 
          " in the $componentsEquations may be incorrect. It has '", valuex,
          "' value.", call. = F)  

sdCoupledModelMsg$verifyModel9 <- function(pcoupledModelId, typeofdres)
  warning("sdsim::verifyModel - Coupled Model '", pcoupledModelId, 
          "' validation: the first element of the coupled model ",
          "definition return value should be a numeric vector ",
          "containg the state variables derivatives. ",
          "Wrong derivative return type: '", typeofdres, "'.", call. = F)

sdCoupledModelMsg$verifyModel10 <- function(pcoupledModelId, dRes, lenst)
  warning("sdsim::verifyModel - Coupled Model '", pcoupledModelId, 
          "' validation: the number of derivatives returned by ", 
          " $componentsEquations (", length(dRes), " - ", 
          paste0(names(dRes), collapse = ', '), 
          ") must equal the length of the initial conditions vector (", 
          lenst, ").", call. = F)

sdCoupledModelMsg$verifyModel11 <- function(pcoupledModelId, typeofdres)
  warning("sdsim::verifyModel - Coupled Model '", pcoupledModelId, 
          "' validation: the coupled model definition function should return ",
          "a list. Wrong return type: '", typeofdres,"'.", call. = F)

sdCoupledModelMsg$verifyModel12 <- function(pcoupledModelId)
  message("sdsim::verifyModel - Coupled Model '", pcoupledModelId, 
          "' Ordinary Differential Equations Validated.")

sdCoupledModelMsg$buildCoupledModel1 <- function(pcoupledModelId)
  warning("sdsim::buildCoupledModel - Coupled Model '", pcoupledModelId, 
          "' build error: no component was added. Add a component ", 
          "before building the coupled model.", call. = F)

sdCoupledModelMsg$buildCoupledModel2 <- function(pcoupledModelId, m, id)
  warning("sdsim::buildCoupledModel - Coupled Model '", pcoupledModelId, 
          "' Error building the connection vectors: The model '", 
          m, "' is not present in the coupled components. Refactor the ",
          "connection to make it valid. Connection '", id, "' skipped.", 
          call. = F)

sdCoupledModelMsg$buildCoupledModel3 <- function(pcoupledModelId, vartype,
                                                 var, m, id)
  warning("sdsim::buildCoupledModel - Coupled Model '", pcoupledModelId, 
          "' Error Building the connection vectors: The ", vartype, " ", 
          var, " from the model '", m, "' do not exist. Refactor ",
          "the connection to make it valid. Connection '", id, 
          "' skipped.", call. = F)

sdCoupledModelMsg$buildCoupledModel4 <- function(pcoupledModelId, m, id)
  warning("sdsim::buildCoupledModel - Coupled Model '", pcoupledModelId, 
          "' Error Building the connection vectors: The ",
          "connected output can only come from the auxiliary ", 
          " equations list (aux$<varName>), from the state ",
          "variables list (st$<varName>) or from the algebric ",
          "equations list (eq$<varName> for static models) of the model '", m, 
          "'. Refactor the connection to make it valid. Connection '", id, 
          "' skipped.", call. = F)

sdCoupledModelMsg$buildCoupledModel5 <- function(pcoupledModelId, in1, u1, m1,
                                                 out2, u2, m2, id)
  warning("sdsim::buildCoupledModel - Coupled Model '", pcoupledModelId, 
          "' Error Building the connection vectors - The input '",
          in1, "' unit (",  u1, ") from the model '", m1,
          "' is different from the output '", out2[[1]], "' '", 
          out2[[2]], "' unit (",  u2,
          ") from the model '", m2,"'. Connection '", id, "' skipped.", 
          call. = F)

sdCoupledModelMsg$buildCoupledModel6 <- function(pcoupledModelId, m1, in1,
                                                 vartype)
  warning("sdsim::buildCoupledModel - Coupled Model '", pcoupledModelId, 
          "' build error: The input '", paste0(m1, ".", in1), 
          "' is connected to more then one ", vartype, 
          ". This input will only receive the last connection. Refactor the ",
          "connections if this is not wanted. ", 
          call. = F)

sdCoupledModelMsg$coupledModelId1 <- function(coupledModelId)
  warning("sdsim::coupledModelId - Missing coupled model ID: It was set to '", 
          coupledModelId, "'.", call. = F)

sdCoupledModelMsg$coupledModelId2 <- function(coupledModelId)
  warning("sdsim::coupledModelId - Invalid coupled model ID type: The coupled ", 
          "model ID must be a string. It was set to '",  coupledModelId, "'.",
          call. = F)

sdCoupledModelMsg$coupledModelDescription <- function(pcoupledModelId)
  warning("sdsim::coupledModelDescription - Invalid coupled model '", 
          pcoupledModelId, "' description type: ",
          "The model description must be a string. No description was set.", 
          call. = F)

sdCoupledModelMsg$defaultCoupledScenario <- function(pcoupledModelId)
  warning("sdsim::defaultCoupledScenario - Coupled Model '", pcoupledModelId, 
          "' get default scenario: The default coupled scenario must be built ",
          "to retrieve it. Build it with the method $buildCoupledModel ",
          "(see help('sdCoupledModelClass')).", call. = F)

sdCoupledModelMsg$connectionsList <- function(pcoupledModelId, vartype)
  warning("sdsim::connectionsList - Coupled Model '", pcoupledModelId, "' ", 
          vartype, " connection list: The default coupled scenario must be ", 
          "built to retrieve the connections. Build it first with the ", 
          "method $buildCoupledModel(see help('sdCoupledModelClass')).", 
          call. = F)

sdCoupledModelMsg$indexComponents <- function(pcoupledModelId)
  warning("sdsim::indexComponents - Coupled Model '", pcoupledModelId,
          "' components index list: The default coupled scenario must be ", 
          "built to retrieve the index list. Build it first with the ", 
          "method $buildCoupledModel (see help('sdCoupledModelClass')).", 
          call. = F)

## FILE sdModel.R
## 
sdModelMsg <- new.env()

sdModelMsg$initialize1 <- function(modelId)
  warning("sdsim::initialize - Model '", modelId, "' initialization: ", 
          "The DifferentialEquations structure does not match specification. ", 
          "It must be a function, see help('sdModel').",
          "Replacement aborted.", call. = F)

sdModelMsg$initialize2 <- function(modelId)
  warning("sdsim::initialize - Model '", modelId, "' initialization: ", 
          "The InitVars structure does not match specification. It ", 
          "must be a function, see help('sdModel').",
          "Replacement aborted.", call. = F)

sdModelMsg$initialize3 <- function(modelId)
  warning("sdsim::initialize - Model '", modelId, "' initialization: ",
          "The PostProcessVars structure does not match specification. It ", 
          "must be a function, see help('sdModel').",
          "Replacement aborted.", call. = F)

sdModelMsg$initialize4 <- function(modelId)
  warning("sdsim::initialize - Model '", modelId, "' initialization: ",
          "The RootSpecification structure does not match specification. It ", 
          "must be a data.frame, a numeric vector or a function, ", 
          "see help('sdModel'). If it is a character or list of ",
          "characters all the components are going to be evaluated or ",
          "converted. Replacement aborted.", call. = F)

sdModelMsg$initialize5 <- function(modelId)
  warning("sdsim::initialize - Model '", modelId, "' initialization: ",
          "The EventFunction structure does not match specification. It ", 
          "must be a function, see help('sdModel').",
          "Replacement aborted.", call. = F)

sdModelMsg$initialize6 <- function(modelId, e)
  warning("sdsim::initialize - Model '", modelId, "' Initialization Error: ", 
          "No auxiliary equations were added. ", e, call. = F)

sdModelMsg$initialize7 <- function(modelId)
  warning("sdsim::initialize - Model '", modelId, "' Initialization: ",
          "Invalid auxiliary equations. See help('sdModel') to ", 
          "learn the accepted types. Replacement aborted.", call. = F)

sdModelMsg$initialize8 <- function(modelId, nameGlobalFuni)
  warning("sdsim::initialize - Model '", modelId, "' Initialization: ",
          "Invalid global function '", nameGlobalFuni, "' skipped. ",
          "All the globalFunctions elements must be functions.", call. = F)

sdModelMsg$initialize9 <- function(modelId)
  warning("sdsim::initialize - Model '", modelId, 
          "' Initialization: The globalFunctions argument must be a named ",
          "list containing only functions.", call. = F)

sdModelMsg$verifyModel0 <- function(pmodelId)
  stop("sdsim::verifyModel - Model '", pmodelId, 
       "' ODE validation aborted: No differential equations function was set.", 
       call. = F)

sdModelMsg$verifyModel1 <- function(pmodelId)
  stop("sdsim::verifyModel - Model '", pmodelId, 
       "' ODE validation aborted: No default scenario was set.", call. = F)

sdModelMsg$verifyModel2 <- function(pmodelId)
  warning("sdsim::verifyModel - Model '", pmodelId, 
          "' validation: No time sequence informed. Define the time sequence ",
          "in the default scenario. Initial time equals 0 will be used.", 
          call. = F)

sdModelMsg$verifyModel3 <- function(pmodelId, auxVar, e)
  warning("sdsim::verifyModel - Model '", pmodelId, 
          "' validation: error evaluating the auxiliary equation '", 
          auxVar, "'. ", e, call. = F)

sdModelMsg$verifyModel4 <- function(pmodelId, auxVar, auxValue)
  warning("sdsim::verifyModel - Model '", pmodelId, 
          "' validation: Evaluation of the auxiliary variable '", auxVar, 
          "' may be incorrect. Value: ", auxValue, ".", call. = F)

sdModelMsg$verifyModel5 <- function(pmodelId, e)
  warning("sdsim::verifyModel - Model '", pmodelId, 
          "' validation: error running the DifferentialEquations. ", e, 
          call. = F)

sdModelMsg$verifyModel6 <- function(pmodelId, xname, x, xvalue)
  warning("sdsim::verifyModel - Model '", pmodelId, 
          "' validation: variable $", xname, " from the '", x,
          "' list in the $DifferentialEquations may be incorrect. ",
          "It has ", xvalue, " value.", call. = F)

sdModelMsg$verifyModel7 <- function(pmodelId, x, xvalue)
  warning("sdsim::verifyModel - Model '", pmodelId, 
          "' validation: variable $", x, 
          " in the $DifferentialEquations may be incorrect. It has ", xvalue, 
          " value.", call. = F) 

sdModelMsg$verifyModel8 <- function(pmodelId, typeofres) 
  warning("sdsim::verifyModel - Model '", pmodelId, 
          "' validation: the first element of the $DifferentialEquations ",
          "return value should be a numeric vector containg the ",
          "state derivatives. Wrong derivative return type: ", typeofres, ".", 
          call. = F)

sdModelMsg$verifyModel9 <- function(pmodelId, dRes, lenst) 
  warning("sdsim::verifyModel - Model '", pmodelId, 
          "' validation: the number of derivatives returned by the ", 
          "$DifferentialEquations (", length(dRes), " - ", 
          paste0(names(dRes), collapse = ', '), 
          ") must equal the length of the initial conditions vector (", 
          lenst, ").", call. = F)

sdModelMsg$verifyModel10 <- function(pmodelId, typeofres) 
  warning("sdsim::verifyModel - Model '", pmodelId, 
          "' validation: the $DifferentialEquations function should return ",
          "a list. Wrong return type:", typeofres,".", call. = F)

sdModelMsg$verifyModel11 <- function(pmodelId) 
  message("sdsim::verifyModel - Model '", pmodelId, 
          "' Ordinary Differential Equations Validated.")

sdModelMsg$verifyModel12 <- function(pmodelId, typeofscen) 
  warning("sdsim::verifyModel - Model '", pmodelId, 
          "' validation: Scenario argument of type '", typeofscen, 
          "' discarted. It must be a valid sdScenarioClass object or ", 
          "a character string with a scenario XML or EXCEL file name.", 
          call. = F)

sdModelMsg$description <- function(pmodelId)
  warning("sdsim::description - Model '", pmodelId, "' get descriptions: ",
          "No default scenario was added, could not get the desription list.", 
          call. = F)

sdModelMsg$unit <- function(pmodelId)
  warning("sdsim::description - Model '", pmodelId, "' get units: ",
          "No default scenario was added, could not get the unit list.", 
          call. = F)

sdModelMsg$modelId1 <- function(modelId)
  warning("sdsim::modelId - Missing model ID: It was set to '", modelId, "'.", 
          call. = F)

sdModelMsg$modelId2 <- function(modelId)
  warning("sdsim::modelId - Invalid model ID type: The model ID must be a ",
          "string. It was set to '", modelId, "'.", call. = F)

sdModelMsg$defaultScenario <- function(pmodelId)
  warning("sdsim::defaultScenario - Model '", pmodelId, 
          "' set default scenario: The default scenario ",
          "must be a sdScenario object or a character string with the name of ", 
          "a scenario XML or Excel file. Create one using the ",
          "help('sdScenario') constructor. No default scenario was set.", 
          call. = F)

sdModelMsg$modelDescription <- function(pmodelId)
  warning("sdsim::modelDescription - Model '", pmodelId, 
          "' set description aborted: The model description ",
          "must be a string.", call. = F)

## FILE sdOutput.R
## 
sdOutputMsg <- new.env()

sdOutputMsg$plot1 <- function(poutputId, plotarg)
  warning("sdsim::plot - Plot output '", poutputId, "': The '...' and the '", 
          plotarg, "' arguments must have the same length. The '", plotarg, 
          "' will not be used.", call. = F)

sdOutputMsg$plot2 <- function(poutputId)
  warning("sdsim::plot - Plot output '", poutputId, "': the argument '...' ",
          "must be a list of character vectors containing ",
          "the formulas with the name of the variables to be plotted. See ",
          "help('sdOutput'). All the output variables will be ploted instead.",
          call. = F)

sdOutputMsg$plot3 <- function(poutputId, xaxis, yaxisArray, namesData)
  warning("sdsim::plot - Plot output '", poutputId, 
          "': Not all the formula variables are valid column names. ",
          "The following variables will be skipped: ", 
          paste(c(xaxis, yaxisArray)[!(c(xaxis, yaxisArray) %in% namesData)], 
                collapse = ", "), call. = F)

sdOutputMsg$plot4 <- function(poutputId, xaxis)
  warning("sdsim::plot - Plot output '", poutputId, "': The x-axis variable '", 
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

sdScenarioMsg$setTimeSequence <- function(pscenarioId, varType)
  warning("sdsim::setTimeSequence - Scenario '", pscenarioId, 
          "' set time sequence: Initial time '", varType, "' not set. ",
          "It must be a numeric value.", call. = F)

sdScenarioMsg$addInput <- function(pscenarioId, namesInterpol, namesInput)
  warning("sdsim::addInput - Scenario '", pscenarioId, 
          "' invalid interpolations: The following time series interpolation ",
          "variables are not present in the input list and will be skipped: ",
          capture.output(namesInterpol[!(namesInterpol %in% namesInput)]),
          call. = F)

sdScenarioMsg$scenarioId <- function(scenarioId)
  warning("sdsim::scenarioId - Invalid scenario ID type: The scenario ID must ",
          "be a string. It was set to '", scenarioId, "'.", call. = F)

sdScenarioMsg$method1 <- function(pscenarioId)
  warning("sdsim::method - Scenario '", pscenarioId,"' set integration method ", 
          "aborted: The 'method' argument must be a string with a valid ",
          "deSolve solver name. Available solver methods: ",
          "'lsoda', 'lsode', 'lsodes', 'lsodar', 'vode', 'daspk', ",
          "'euler', 'rk4', 'ode23', 'ode45', 'radau', 'bdf', 'bdf_d', ",
          "'adams', 'impAdams', 'impAdams_d'. The default method ",
          "'lsoda' was set.", call. = F)

sdScenarioMsg$method2 <- function(pscenarioId)
  warning("sdsim::method - Scenario '",pscenarioId,"' set integration method: ", 
          "The given method is not valid. Available solver methods: ",
          "'lsoda', 'lsode', 'lsodes', 'lsodar', 'vode', 'daspk', ",
          "'euler', 'rk4', 'ode23', 'ode45', 'radau', 'bdf', 'bdf_d', ",
          "'adams', 'impAdams', 'impAdams_d'. The default method ",
          "'lsoda' was set.", call. = F)


sdScenarioMsg$times <- function(pscenarioId)
  warning("sdsim::times - Scenario '", pscenarioId, "' set time sequence: ",
          "The simulation time sequence must be a numeric named list ",
          "with at least one of the following three elements: 'from', ",
          "'to' and 'by', representing the initial time, the final ",
          "time and the simulation time step. ",
          "Scenario initialized without time sequence.", call. = F)

sdScenarioMsg$description <- function(pscenarioId, typeofdescription)
  warning("sdsim::description - Scenario '", pscenarioId, 
          "' set description aborted: ",
          "Description type (", typeofdescription,
          ") not supported. It should be a named list.", call. = F)

sdScenarioMsg$unit <- function(pscenarioId, typeofunit)
  warning("sdsim::unit - Scenario '", pscenarioId, 
          "' set unit aborted: ",
          "Unit type (", typeofunit,
          ") not supported. It should be a named list. ", call. = F)

sdScenarioMsg$addVar1 <- function(pscenarioId)
  warning("sdsim::addVar - Scenario '", pscenarioId, 
          "' add variable aborted: All the scenario variables must be named.",
          call. = F)

sdScenarioMsg$addVar2 <- function(pscenarioId)
  warning("sdsim::addVar - Scenario '", pscenarioId, 
          "' add variable: Unnamed variable skipped. ",
          "All the scenario variables must be named.", call. = F)

sdScenarioMsg$addVar3 <- function(pscenarioId, varType, var)
  warning("sdsim::addVar - Scenario '", pscenarioId, "' add variable: ",
          paste(gsub("(^.)", "\\U\\1", varType, perl = T), 
                "values should be numeric. Variable "), var, 
          " will be skipped.", call. = F)

sdScenarioMsg$addVar4 <- function(pscenarioId, varType, var, varValue)
  message("sdsim::addVar - Scenario '", pscenarioId, 
          "' add variable: ",
          gsub("(^.)", "\\U\\1", varType, perl = T), " of ", var, " set to ", 
          capture.output(varValue))

sdScenarioMsg$addVar5 <- function(pscenarioId, varType, var, varValue)
  message("sdsim::addVar - Scenario '", pscenarioId, 
          "' add variable: ", "Value of ", varType, " '", var, "' set to ", 
          capture.output(varValue))

sdScenarioMsg$addVar6 <- function(pscenarioId, varType, var, varValue)
  warning("sdsim::addVar - Scenario '", pscenarioId, 
          "' add variable: The ", varType, " '", var, 
          "' already exists in this scenario. It will be reset to ", 
          capture.output(varValue), ".", call. = F)

sdScenarioMsg$addVar7 <- function(pscenarioId, varType, var)
  warning("sdsim::addVar - Scenario '", pscenarioId, 
          "' add variable: ", paste(gsub("(^.)", "\\U\\1", varType, perl = T), 
                                    "values should not be null. Variable "), 
          var, " will be skipped.", call. = F)

sdScenarioMsg$removeVar1 <- function(pscenarioId, varType, typeofx)
  warning("sdsim::removeVar - Scenario '", pscenarioId, 
          "' remove ", varType,
          ": the variable name must be a string, wrong type: ", typeofx, 
          call. = F)

sdScenarioMsg$removeVar2 <- function(pscenarioId, varType, var)
  message("sdsim::removeVar - Scenario '", pscenarioId, 
          "' remove variable: ", gsub("(^.)", "\\U\\1", varType, perl = T), 
          " variable ", var, " removed")

sdScenarioMsg$removeVar3 <- function(pscenarioId, varType, var)
  warning("sdsim::removeVar - Scenario '", pscenarioId, 
          "' remove variable: ", gsub("(^.)", "\\U\\1", varType, perl = T), 
          " variable ", var, " not found.", call. = F) 

## FILE sdStaticModel.R
##
sdStaticModelMsg <- new.env()

sdStaticModelMsg$initialize1 <- function(staticModelId)
  warning("sdsim::initialize - Static Model '", staticModelId, 
          "' initialization: ", 
          "The InitVars structure does not match specification. It ", 
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

sdStaticModelMsg$staticModelId1 <- function(staticModelId)
  warning("sdsim::staticModelId - Missing static model ID: It was set to '", 
          staticModelId, "'.", call. = F)

sdStaticModelMsg$staticModelId2 <- function(staticModelId)
  warning("sdsim::staticModelId - Invalid static model ID type: ",
          "The model ID must be a string. It was set to '", staticModelId, "'.", 
          call. = F)

sdStaticModelMsg$defaultscenario1 <- function(pstaticModelId)
  warning("sdsim::defaultScenario - Static Model '", pstaticModelId, 
          "' set default scenario: ",
          "static models do not have state variables. All the state ", 
          "variables were removed before setting the default scenario.",
          call. = F)

sdStaticModelMsg$defaultscenario2 <- function(pstaticModelId)
  warning("sdsim::defaultScenario - Static Model '", pstaticModelId, 
          "' set default scenario: ",
          "The default scenario must be a sdScenario object. ", 
          "Create one using the help('sdScenario') or help('sdLoadScenario')",
          "constructors. No default scenario was set.", call. = F)

sdStaticModelMsg$staticModelDescription <- function(pstaticModelId)
  warning("sdsim::staticModelDescription - Static Model '", pstaticModelId, 
          "' set description aborted: The model description must be a string.",
          call. = F)

## FILE sdSimulator.R
##
sdSimulatorMsg <- new.env()

sdSimulatorMsg$sdSimulate <- function()
  stop("sdsim::sdSimulate - Simulation aborted: ",
       "A model must be informed to run the simulation.", call. = F)

sdSimulatorMsg$sdSimulateAtomic0 <- function(modelId)
  stop("sdsim::sdSimulate - Simulation of the model '", modelId,"' aborted: ",
       "A valid model default scenario or the scenario argument must ",
       "be informed to run the simulation.", call. = F)

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

sdSimulatorMsg$sdSimulateAtomic7 <- function(modelId)
  stop("sdsim::sdSimulate - Simulation of the model '", modelId,
       "' aborted: No differential equations function was set.", 
       call. = F)

sdSimulatorMsg$sdSimulateStatic1 <- function(staticModelId)
  stop("sdsim::sdSimulate - Simulation of the static model '", staticModelId,
       "' aborted: No time sequence informed. Define the time sequence ",
       "in the default scenario or via the arguments.", call. = F)

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