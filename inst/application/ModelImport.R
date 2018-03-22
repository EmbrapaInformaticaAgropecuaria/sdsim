# Loads model if no other models with same ids as the one loading exist
# Else, query user for confirmation before overwriting
LoadModel <- function(modelName, simData, session, input, output, 
                         repository = NULL, loadDefaultScenario = T, 
                         nTableRows = 50, replaceId = NULL) {
  model <- ParseXML(modelName, repository)
  
  id <- ""
  componentIds <- list()
  if(!is.null(model$modelId)) {
    if(!is.null(replaceId) && replaceId != "")
      model$modelId <- replaceId
    id <- model$modelId
  } else if (!is.null(model$coupledModelId)) {
    if(!is.null(replaceId) && replaceId != "")
      model$coupledModelId <- replaceId
    id <- model$coupledModelId
    componentIds <- lapply(model$components, function(x) {
      if(!is.null(x$modelId))
        x$modelId
      else if(!is.null(x$staticModelId))
        x$staticModelId
      else if(!is.null(x$coupledModelId))
        x$coupledModelId
    })
  } else if (!is.null(model$staticModelId)) {
    if(!is.null(replaceId) && replaceId != "")
      model$staticModelId <- replaceId
    id <- model$staticModelId
  }
  
  modelIds <- c(id, componentIds)
  
  if(any(modelIds %in% names(simData$models))) {
    # Save loading model info in simData
    simData$loadingModel <- model
    
    idsToOverwrite <- paste(modelIds[modelIds %in% names(simData$models)], collapse = "\", \"")
    idsToOverwrite <- sub("(.*),", "\\1 and", idsToOverwrite)
    message <- paste0("The model(s) \"", idsToOverwrite, "\" will be overwritten.",
                     "\nDo you wish to continue?")
    responseInputName <- "confirmModelOverwrite"
    session$sendCustomMessage("confirmOverwrite", 
                              list(message = message, 
                                   responseInputName = responseInputName))
    return(NULL)
  } else {
    msg <- ConfirmLoadModel(model, simData, session, input, 
                     output, nTableRows = nTableRows)
    return(msg)
  }
}

ConfirmLoadModel <- function(model, simData, session, input, output, 
                      loadDefaultScenario = T, nTableRows = 50) {
  withCallingHandlers({
    tryCatch({
      if(!is.null(model$modelId)) {
        msg <- LoadAtomicModel(model, simData, session, input, output, 
                        loadDefaultScenario)
      } else if(!is.null(model$staticModelId)) {
        msg <- LoadStaticModel(model, simData, session, input, output, 
                        loadDefaultScenario)
      } else if (!is.null(model$coupledModelId)){
        msg <- LoadCoupledModel(model, simData, session, input, output, 
                        loadDefaultScenario)
      } else {
        # Invalid model file
        msg <- list(
          paste("Load model aborted. The given file's model type is invalid.",
                "Generate your XML files using the sdsim package functions."),
          "red"
        )
      }
      
      # Update UI
      UpdateLoadedModel(simData, session, input, output, nTableRows)
      UpdateLoadedScenario(simData, session, input, output, nTableRows)
      
      return(msg)
    },
    error = function(e) {
      errorOutput <- paste(capture.output(e), collapse = " ")
      return(list(errorOutput, "red"))
    })
  },
  warning = function(w) {
    warningOutput <- paste(capture.output(w), collapse = " ")
    return(list(warningOutput, "red"))
  })
}

LoadAtomicModel <- function(model, simData, session, input, output, 
                            loadDefaultScenario = T) {
  # If default scenario is null or shouldn't be loaded
  if(!loadDefaultScenario || is.null(model$defaultScenario)) {
    # Load empty scenario as default
    scenario <- ParseXML("UnnamedScenario", "application/xml")
    model$defaultScenario <- scenario
  }
  
  model$defaultScenario$scenarioId <- "Default"
  # Load model data and save to reactive list
  simData$models[[model$modelId]] <- LoadAtomicModelData(model, simData)
  
  # Load default scenario
  LoadScenarioData(model$defaultScenario, simData, model$modelId)
  
  # Update current model
  simData$currentModelId <- model$modelId
  
  msg <- list(
    paste(model$modelId, "model successfully loaded!"),
    "green"
  )
  return(msg)
}

LoadStaticModel <- function(model, simData, session, input, output, 
                            loadDefaultScenario = T) {
  # Load static model
  
  # If default scenario is null or shouldn't be loaded
  if(!loadDefaultScenario || is.null(model$defaultScenario)) {
    # Load empty scenario as default
    scenario <- ParseXML("UnnamedScenario", "application/xml")
    scenario$scenarioId <- "Default"
    model$defaultScenario <- scenario
  }
  
  model$defaultScenario$scenarioId <- "Default"
  # Load static model
  simData$models[[model$staticModelId]] <- LoadStaticModelData(model, simData)
  
  LoadScenarioData(model$defaultScenario, simData, model$staticModelId)
  
  # Update current model
  simData$currentModelId <- model$staticModelId
  
  msg <- list(
    paste(model$staticModelId, "model successfully loaded!"),
    "green"
  )
  return(msg)
}

LoadCoupledModel <- function(model, simData, session, input, output, 
                             loadDefaultScenario = T) {
  # Load coupled model
  componentIdList <- c()
  # Loads all component models into model list
  for(component in model$components) {
    if(!is.null(component$modelId)) {
      componentIdList <- c(componentIdList, component$modelId)
      # If default scenario is null
      if(is.null(component$defaultScenario)) {
        # Load empty scenario as default
        scenario <- ParseXML("UnnamedScenario", "application/xml")
        component$defaultScenario <- scenario
      }
      
      component$defaultScenario$scenarioId <- "Default"
      simData$models[[component$modelId]] <- LoadAtomicModelData(component, simData)
      
      # Load default scenario and save it to the component's scenario list
      LoadScenarioData(component$defaultScenario, simData, component$modelId)
      
    } else if(!is.null(component$staticModelId)) {
      componentIdList <- c(componentIdList, component$staticModelId)
      # If default scenario is null
      if(is.null(component$defaultScenario)) {
        # Load empty scenario as default
        scenario <- ParseXML("UnnamedScenario", "application/xml")
        component$defaultScenario <- scenario
      }
      
      component$defaultScenario$scenarioId <- "Default"
      simData$models[[component$staticModelId]] <- LoadStaticModelData(component, simData)
      
      # Load default scenario and save it to the component's scenario list
      LoadScenarioData(component$defaultScenario, simData, component$staticModelId)
    } else if (!is.null(component$coupledModelId)){
      # msg <- LoadCoupledModel(model, simData, session, input, output, 
      # loadDefaultScenario)
      # TODO
    }
  }
  
  # Set empty scenario as current scenario
  scenario <- ParseXML("UnnamedScenario", "application/xml")
  
  # Get component Ids data frame
  model$componentIds <- data.frame('Component ID' = componentIdList,
                                   stringsAsFactors = FALSE, 
                                   row.names = NULL, check.names = F)
  
  simData$models[[model$coupledModelId]] <- 
    LoadCoupledModelData(model, simData, scenario$scenarioId)
  
  LoadScenarioData(scenario, simData, model$coupledModelId)
  
  # Update current model
  simData$currentModelId <- model$coupledModelId
  
  msg <- list(
    paste(model$coupledModelId, "model successfully loaded!"),
    "green"
  )
  return(msg)
}

LoadScenario <- function(scenarioName, simData, session, input, output, 
                         repository = NULL, nTableRows = 50, replaceId = NULL) {
  withCallingHandlers({
    tryCatch({
      if(!is.null(repository) || grepl(".[xX][mM][lL]$", scenarioName)) {
        scenario <- ParseXML(scenarioName, repository)
      } else if(grepl(".[xX][lL][sS][xX]$", scenarioName)) {
        scenario <- ParseXlsx(scenarioName)
      }
      
      if(!is.null(replaceId)) {
        scenario$scenarioId <- replaceId
      }
      
      if(scenario$scenarioId == "Default")
        return(list("Cannot create or load a scenario with ID \"Default\"!",
                    "red"))
      
      currentModel <- simData$models[[simData$currentModelId]]
      currentModelScenarioIds <- names(currentModel$scenarios)
      
      if(scenario$scenarioId %in% currentModelScenarioIds) {
        # Save loading scenario info in simData
        simData$loadingScenario <- scenario
        
        message <- paste0("The current model's scenario \"", scenario$scenarioId, 
                          "\" will be overwritten.",
                          "\nDo you wish to continue?")
        responseInputName <- "confirmScenarioOverwrite"
        session$sendCustomMessage("confirmOverwrite", 
                                  list(message = message, 
                                       responseInputName = responseInputName))
        return(NULL)
      } else {
        msg <- ConfirmLoadScenario(scenario, simData, session, input, output, 
                                   repository, nTableRows)
        return(msg)
      }
    },
    error = function(e) {
      errorOutput <- paste(capture.output(e), collapse = " ")
      return(list(errorOutput, "red"))
    })
  },
  warning = function(w) {
    warningOutput <- paste(capture.output(w), collapse = " ")
    return(list(warningOutput, "red"))
  })
}

ConfirmLoadScenario <- function(scenario, simData, session, input, output, 
                         repository = NULL, nTableRows = 50) {
  withCallingHandlers({
    tryCatch({
      # Load scenario data and add it to current model in simData
      LoadScenarioData(scenario, simData, simData$currentModelId)
      
      # Change the current model's current scenario to the loaded scenario
      simData$models[[simData$currentModelId]]$currentScenarioId <- scenario$scenarioId
      
      msg <- paste(scenario$scenarioId, "scenario successfully loaded!")
      
      UpdateLoadedScenario(simData, session, input, output, nTableRows)
      
      return(list(msg, "green"))
    },
    error = function(e) {
      errorOutput <- paste(capture.output(e), collapse = " ")
      return(list(errorOutput, "red"))
    })
  },
  warning = function(w) {
    warningOutput <- paste(capture.output(w), collapse = " ")
    return(list(warningOutput, "red"))
  })
}

ParseXlsx <- function(file) {
  scenario <- ReadDataExcel(file)
  
  interpolation <- DataFrameToList(scenario$input, valueCol = "Interpolation")
  interpolation <- interpolation[which(interpolation != "")]
  
  units <- as.list(unlist(lapply(list("state", "constant", "input", "parameter", "switch", "aux"), function(x) {
    DataFrameToList(scenario[[x]], valueCol = "Unit")
  })))
  units <- units[which(units != "")]
  
  descriptions <- as.list(unlist(lapply(list("state", "constant", "input", "parameter", "switch", "aux"), function(x) {
    DataFrameToList(scenario[[x]], valueCol = "Description")
  })))
  descriptions <- descriptions[which(descriptions != "")]
  
  method <- GetDataFrameValue(scenario$simulation, "method", "Variable", "Value")
  from <- GetDataFrameValue(scenario$simulation, "from", "Variable", "Value")
  to <- GetDataFrameValue(scenario$simulation, "to", "Variable", "Value")
  by <- GetDataFrameValue(scenario$simulation, "by", "Variable", "Value")
  scenarioId <- GetDataFrameValue(scenario$simulation, "scenarioId", "Variable", "Value")
  
  lscenario <- list(scenarioId = scenarioId,
                    times = list(from = from, to = to, by = by),
                    method = method,
                    state = DataFrameToList(scenario$state),
                    constant = DataFrameToList(scenario$constant),
                    input = DataFrameToList(scenario$input),
                    interpolation = interpolation,
                    parameter = DataFrameToList(scenario$parameter),
                    switch = DataFrameToList(scenario$switch),
                    unit = units,
                    description = descriptions)
  
  return(lscenario)
}

ParseXML <- function(file, repositoryDir = NULL) {
  if(!is.null(repositoryDir)) {
    file <- system.file(appDir = paste0(repositoryDir, "/", file, ".xml"), 
                        package = "sdsim")
  }

  sdsimprefix <- paste(readLines(file, n = 3), collapse = "\n")
  if (!grepl(pattern = "<\\?sdsim.*version.*\\?>", sdsimprefix, ignore.case = T))
    stop(paste("Load model aborted. The given file is not a valid XML file.",
               "Generate your XML files using the sdsim package functions."),
         call. = F)
  
  # else
  # {
  #   # valid prefix, now check version
  #   if (!grepl(pattern = paste0("(?<=version=\\')", 
  #                               sub("\\.","\\\\.", packageVersion("sdsim"))), 
  #              x = sdsimprefix, ignore.case = T, perl = T))
  #     warning("Load Model: The sdsim XML version is deprecated. The current ",
  #             "sdsim version is: ", packageVersion("sdsim"))
  # }
  
  data <- XML::xmlParse(file)
  data <- XML::xmlToList(data)
  return(data)
}

UpdateLoadedModel <- function(simData, session, input, output, nTableRows = 50) {
  # Clear previous model simulation results and log
  ClearSimulationResults(simData, session, input, output)
  
  # Get current model
  currentModel <- simData$models[[simData$currentModelId]]
  
  if(currentModel$type == "atomic") {
    # Unhide atomic model panel
    session$sendCustomMessage("unhideElement", "atomicModelPage")
    # Hide static model panel
    session$sendCustomMessage("hideElement", "staticModelPage")
    # Hide coupled model panel
    session$sendCustomMessage("hideElement", "coupledModelPage")
    # Unhide method, initialTime, finalTime and step inputs
    session$sendCustomMessage("unhideElement", "simulationParametersDiv")
    
    # Clear other model UI's
    ClearStaticModelUI(simData, session, input, output)
    ClearCoupledModelUI(simData, session, input, output)
    
    # Update scripts
    CustomAceUpdate(session, "description", value = currentModel$description)
    CustomAceUpdate(session, "DifferentialEquations", value = currentModel$DifferentialEquations)
    CustomAceUpdate(session, "initVars", value = currentModel$initVars)
    CustomAceUpdate(session, "root", value = currentModel$root)
    CustomAceUpdate(session, "event", value = currentModel$event)
    CustomAceUpdate(session, "globalFunctions", value = paste(currentModel$globalFunctions, collapse = "\n\n"))
    
    # Update auxiliaries table
    nRows <- nTableRows - NROW(currentModel$aux)
    aux <- rbind(currentModel$aux, CreateVarDataFrame(nRows = nRows),
                 stringsAsFactors = FALSE, row.names = NULL)
    
    UpdateRHandsontable(aux, "aux", output)
    
    simData$changed$aux <- F
  } else if(currentModel$type == "static") {
    # Hide atomic model panel
    session$sendCustomMessage("hideElement", "atomicModelPage")
    # Unhide static model panel
    session$sendCustomMessage("unhideElement", "staticModelPage")
    # Hide coupled model panel
    session$sendCustomMessage("hideElement", "coupledModelPage")
    # Hide method, initialTime, finalTime and step inputs
    session$sendCustomMessage("hideElement", "simulationParametersDiv")
    
    # Clear other model UI's
    ClearAtomicModelUI(simData, session, input, output)
    ClearCoupledModelUI(simData, session, input, output)
    
    # Update scripts
    CustomAceUpdate(session, "description", value = currentModel$description)
    CustomAceUpdate(session, "staticInitVars", value = currentModel$initVars)
    CustomAceUpdate(session, "staticGlobalFunctions", value = paste(currentModel$globalFunctions, collapse = "\n\n"))
    
    # Update auxiliaries table
    nRows <- nTableRows - NROW(currentModel$aux)
    aux <- rbind(currentModel$aux, CreateVarDataFrame(nRows = nRows),
                 stringsAsFactors = FALSE, row.names = NULL)
    
    UpdateRHandsontable(aux, "staticAux", output)
    simData$changed$staticAux <- F
  } else if(currentModel$type == "coupled") {
    # Hide atomic model panel
    session$sendCustomMessage("hideElement", "atomicModelPage")
    # Hide static model panel
    session$sendCustomMessage("hideElement", "staticModelPage")
    # Unhide coupled model panel
    session$sendCustomMessage("unhideElement", "coupledModelPage")
    # Unhide method, initialTime, finalTime and step inputs
    session$sendCustomMessage("unhideElement", "simulationParametersDiv")
    
    # Clear other model UI's
    ClearAtomicModelUI(simData, session, input, output)
    ClearStaticModelUI(simData, session, input, output)
    
    # Update coupled model connections
    if(!is.null(currentModel$connections)){
      nRows <- nTableRows - NROW(currentModel$connections)
      connections <- rbind(currentModel$connections, 
                           CreateConnectionsDataFrame(nRows = nRows),
                           stringsAsFactors = FALSE, row.names = NULL)
    } else {
      nRows <- nTableRows
      connections <- CreateConnectionsDataFrame(nRows = nRows)
    }
    
    UpdateRHandsontable(connections, "connections", output)
    simData$changed$connections <- F
    
    # Update coupled model components
    if(!is.null(currentModel$componentIds)){
      nRows <- nTableRows - NROW(currentModel$componentIds)
      componentIds <- rbind(currentModel$componentIds, 
                            CreateComponentsDataFrame(nRows = nRows),
                            stringsAsFactors = FALSE, row.names = NULL)
    } else {
      nRows <- nTableRows
      componentIds <- CreateComponentsDataFrame(nRows = nRows)
    }
    
    UpdateRHandsontable(componentIds, "componentIds", output)
    simData$changed$componentIds <- F
  } 
}

# SaveModelSimulationResults <- function(simData, session, input, output) {
#   # Clear simulation log
#   output$errorTitle <- renderText("")
#   output$errorLog <- renderText("")
#   
#   
#   
#   updateSelectInput(session, "selVarPlot", choices = "No Variables Available", selected = "No Variables Available")
#   updateSelectInput(session, "selectXAxisPlot", choices = "No Variables Available", selected = "No Variables Available")
#   updateTextInput(session, "plotTitle", value = "")
#   updateTextInput(session, "plotXLabel", value = "")
#   updateTextInput(session, "plotYLabel", value = "")
# }

ClearSimulationResults <- function(simData, session, input, output) {
  # Clear previous model simulation results
  updateSelectInput(session, "selVarPlot", choices = "No Variables Available", selected = "No Variables Available")
  updateSelectInput(session, "selectXAxisPlot", choices = "No Variables Available", selected = "No Variables Available")
  updateTextInput(session, "plotTitle", value = "")
  updateTextInput(session, "plotXLabel", value = "")
  updateTextInput(session, "plotYLabel", value = "")

  # Clear simulation log
  output$errorTitle <- renderText("")
  output$errorLog <- renderText("")
}

ClearAtomicModelUI <- function(simData, session, input, output) {
  # Clear atomic model script fields
  CustomAceUpdate(session, "DifferentialEquations", value = "")
  CustomAceUpdate(session, "initVars", value = "")
  CustomAceUpdate(session, "root", value = "")
  CustomAceUpdate(session, "event", value = "")
  CustomAceUpdate(session, "globalFunctions", value = "")
  
  # Clear atomic model aux table
  emptyDF <- CreateVarDataFrame(nRows = 1)
  UpdateRHandsontable(emptyDF, "aux", output)
  simData$changed$aux <- F
}

ClearStaticModelUI <- function(simData, session, input, output) {
  # Clear static model script fields
  CustomAceUpdate(session, "staticInitVars", value = "")
  CustomAceUpdate(session, "staticGlobalFunctions", value = "")
  
  # Clear static model aux table
  emptyDF <- CreateVarDataFrame(nRows = 1)
  UpdateRHandsontable(emptyDF, "staticAux", output)
  simData$changed$staticAux <- F
}

ClearCoupledModelUI <- function(simData, session, input, output) {
  # Clear coupled model connections table
  emptyDF <- CreateConnectionsDataFrame(nRows = 50)
  UpdateRHandsontable(emptyDF, "connections", output)
  emptyDF <- CreateComponentsDataFrame(nRows = 50)
  UpdateRHandsontable(emptyDF, "components", output)
  simData$changed$components <- F
}

UpdateLoadedScenario <- function(simData, session, input, output, nTableRows = 50) {
  currentModel <- simData$models[[simData$currentModelId]]

  currentScenario <- NULL
  
  if(!is.null(currentModel) && 
     !is.null(currentModel$scenarios) &&
     !is.null(currentModel$currentScenarioId))
    currentScenario <- currentModel$scenarios[[currentModel$currentScenarioId]]
  
  if(!is.null(currentScenario)) {
    updateTextInput(session, "initialTime", 
                    label = "Initial Time", 
                    value = currentScenario$from)
    
    updateTextInput(session, "finalTime", 
                    label = "Final Time", 
                    value = currentScenario$to)
    
    updateTextInput(session, "step", 
                    label = "Time Step", 
                    value = currentScenario$by)
    
    updateSelectInput(session = session, inputId = "method", 
                      selected = currentScenario$method,
                      choices = c("lsoda", "lsode", "lsodes", "lsodar", 
                                  "vode", "daspk", "euler", "rk4", "ode23", 
                                  "ode45", "radau", "bdf", "bdf_d", "adams", 
                                  "impAdams", "impAdams_d"))
    
    nRows <- nTableRows - NROW(currentScenario$state)
    state <- rbind(currentScenario$state, CreateVarDataFrame(nRows = nRows),
                   stringsAsFactors = FALSE, row.names = NULL)
    
    nRows <- nTableRows - NROW(currentScenario$constant)
    constant <- rbind(currentScenario$constant, CreateVarDataFrame(nRows = nRows),
                      stringsAsFactors = FALSE, row.names = NULL)
    
    nRows <- nTableRows - NROW(currentScenario$input)
    input <- rbind(currentScenario$input, CreateInputDataFrame(nRows = nRows),
                   stringsAsFactors = FALSE, row.names = NULL)
    
    nRows <- nTableRows - NROW(currentScenario$parameter)
    parameter <- rbind(currentScenario$parameter, 
                       CreateVarDataFrame(nRows = nRows),
                       stringsAsFactors = FALSE, row.names = NULL)
    
    nRows <- nTableRows - NROW(currentScenario$switch)
    switch <- rbind(currentScenario$switch, CreateVarDataFrame(nRows = nRows),
                    stringsAsFactors = FALSE, row.names = NULL)
    
    simData$changed$state <- F
    simData$changed$constant <- F
    simData$changed$input <- F
    simData$changed$parameter <- F
    simData$changed$switch <- F
    
    UpdateRHandsontable(state, "state", output)
    UpdateRHandsontable(constant, "constant", output)
    UpdateRHandsontable(input, "input", output)
    UpdateRHandsontable(parameter, "parameter", output)
    UpdateRHandsontable(switch, "switch", output)
  }
}

# Loads an atomic model into the UI format
LoadAtomicModelData <- function(model, simData) {
  # Get auxiliary data frame and update rhandsontable values
  aux <- AuxListToDataFrame(model)
  
  globalFunctions <- model$GlobalFunctions
  globalFunctions <- lapply(names(globalFunctions), function(x) {
    paste0(x, " <- ", FunToString(globalFunctions[[x]]))
  })
  
  # Create model
  modelData <- CreateModelObject(
    modelId = model$modelId, 
    description = model$modelDescription, 
    DifferentialEquations = FunToString(model$DifferentialEquations), 
    initVars = FunToString(model$InitVars), 
    root = FunToString(model$RootSpecification), 
    event = FunToString(model$EventFunction),
    aux = aux,
    globalFunctions = globalFunctions,
    defaultScenarioId = model$defaultScenario$scenarioId,
    currentScenarioId = model$defaultScenario$scenarioId
  )
  
  return(modelData)
}

# Loads a static model into the UI format
LoadStaticModelData <- function(model, simData) {
  # Get auxiliary data frame and update rhandsontable values
  aux <- AuxListToDataFrame(model, "equations")
  
  globalFunctions <- model$GlobalFunctions
  globalFunctions <- lapply(names(globalFunctions), function(x) {
    paste0(x, " <- ", FunToString(globalFunctions[[x]]))
  })
  
  # Create model
  modelData <- CreateStaticModelObject(
    modelId = model$staticModelId, 
    description = model$modelDescription,
    initVars = FunToString(model$InitVars),
    aux = aux,
    globalFunctions = globalFunctions,
    defaultScenarioId = model$defaultScenario$scenarioId,
    currentScenarioId = model$defaultScenario$scenarioId
  )
  
  return(modelData)
}

# Loads an coupled model into the UI format
LoadCoupledModelData <- function(model, simData, currentScenarioId = NULL) {
  connections <- ConnectionsListToDataFrame(model)
  modelData <- CreateCoupledModelObject(
    modelId = model$coupledModelId,
    description = model$coupledModelDescription,
    connections = connections,
    componentIds = model$componentIds,
    currentScenarioId = currentScenarioId
  )
  return(modelData)
}

# Loads a scenario from a list into the UI
LoadScenarioData <- function(scenario, simData, parentModelId = NULL) {
  # Get variables data frames from lists
  state <- ScenarioListToDataFrame(scenario, "state")
  constant <- ScenarioListToDataFrame(scenario, "constant")
  input <- ScenarioListToDataFrame(scenario, "input")
  parameter <- ScenarioListToDataFrame(scenario, "parameter")
  switch <- ScenarioListToDataFrame(scenario, "switch")
  
  times <- scenario$times
  
  # Create scenario
  scenarioData <- CreateScenarioObject(
    scenarioId = scenario$scenarioId,
    from = times$from,
    to = times$to,
    by = times$by,
    method = scenario$method,
    state = state,
    constant = constant,
    input = input,
    parameter = parameter,
    switch = switch
  )
  
  # Save scenario to parent model scenario list
  if(!is.null(parentModelId)) {
    simData$models[[parentModelId]]$scenarios[[scenario$scenarioId]] <- scenarioData
  }
  return(scenarioData)
}



CreateModelObject <- function(modelId,
                              description = NULL,
                              DifferentialEquations = NULL,
                              initVars = NULL,
                              root = NULL,
                              event = NULL,
                              aux = NULL,
                              globalFunctions = NULL,
                              defaultScenarioId = NULL,
                              currentScenarioId = NULL,
                              scenarios = list()) {
  model <- list()
  
  if(!is.null(DifferentialEquations))
    model$type <- "atomic"
  else
    model$type <- "static"
  
  model$modelId <- modelId
  model$description <- description
  model$DifferentialEquations <- DifferentialEquations
  model$initVars <- initVars
  model$root <- root
  model$event <- event
  model$aux <- aux
  model$globalFunctions <- globalFunctions
  model$defaultScenarioId <- defaultScenarioId
  model$currentScenarioId <- currentScenarioId
  model$scenarios <- scenarios
  
  return(model)
}

CreateStaticModelObject <- function(modelId,
                              description = NULL,
                              initVars = NULL,
                              aux = NULL,
                              globalFunctions = NULL,
                              defaultScenarioId = NULL,
                              currentScenarioId = NULL,
                              scenarios = list()) {
  model <- list()
  
  model$type <- "static"
  
  model$modelId <- modelId
  model$description <- description
  model$initVars <- initVars
  model$aux <- aux
  model$globalFunctions <- globalFunctions
  model$defaultScenarioId <- defaultScenarioId
  model$currentScenarioId <- currentScenarioId
  model$scenarios <- scenarios
  
  return(model)
}

CreateCoupledModelObject <- function(modelId = NULL,
                                     description = NULL,
                                     componentIds = NULL,
                                     connections = NULL,
                                     currentScenarioId = NULL,
                                     scenarios = list()) {
  model <- list()
  
  model$type <- "coupled"
  model$modelId <- modelId
  model$description <- description
  model$componentIds <- componentIds
  model$connections <- connections
  model$currentScenarioId <- currentScenarioId
  model$scenarios <- scenarios
  
  return(model)
}

CreateScenarioObject <- function(scenarioId = "default scenario",
                                 from = 0, to = 100, by = 1,
                                 method = "lsoda",
                                 state = NULL,
                                 constant = NULL,
                                 input = NULL,
                                 parameter = NULL,
                                 switch = NULL) {
  scenario <- list()
  
  scenario$id <- scenarioId
  
  scenario$from <- from
  scenario$to <- to
  scenario$by <- by
  scenario$method <- method
  
  scenario$state <- state
  scenario$constant <- constant
  scenario$input <- input
  scenario$parameter <- parameter
  scenario$switch <- switch
  
  return(scenario)
}


ConnectionsListToDataFrame <- function(model, connectionsListName = "connections") {
  if(!is.null(model[[connectionsListName]])) {
    # convert the connections (parse text)
    connections <- lapply(model[[connectionsListName]], function(x)
    {
      if (is.character(x) && length(x) == 1)
        eval(parse(text = x))
      else
        x
    })
    
    connectionsDF <- t(as.data.frame(connections, stringsAsFactors = FALSE, row.names = NULL))
    row.names(connectionsDF) <- c()
    colnames(connectionsDF) <- c('Connection ID', 
                                 'Receiver Component ID', 
                                 'Receiver Input',
                                 'Sender Component ID',
                                 'Sender Output')
    
    return(connectionsDF)
  } else {
    NULL
  }
  
}

AuxListToDataFrame <- function(model, auxListName = "aux") {
  ls <- lapply(model[[auxListName]], function(x) toString(x))
  
  description <- model$defaultScenario$description
  unit <- model$defaultScenario$unit
  
  df <- data.frame(Variable = names(ls), Value = unlist(ls),
                   Unit = character(NROW(ls)),
                   Description = character(NROW(ls)),
                   stringsAsFactors = FALSE, row.names = NULL)
  
  # Add descriptions and units to each variable
  for (varNm in df[["Variable"]]) {
    if (varNm %in% names(description))
      df[["Description"]][[which(df[["Variable"]] == varNm)]] <- description[[varNm]]
    if (varNm %in% names(unit))
      df[["Unit"]][[which(df[["Variable"]] == varNm)]] <- unit[[varNm]]
  }
  
  return(df)
}

ScenarioListToDataFrame <- function(scenario, listName) {
  ls <- lapply(scenario[[listName]], function(x) toString(x))
  
  interpolation <- scenario$interpolation
  description <- scenario$description
  unit <- scenario$unit

  variableNames <- names(ls)
  if(is.null(variableNames))
    variableNames <- character(0)
  
  values <- unlist(ls)
  if(is.null(values))
    values <- character(0)

  if(listName != "input") {
    df <- data.frame(Variable = variableNames, Value = values,
                     Unit = character(NROW(ls)),
                     Description = character(NROW(ls)),
                     stringsAsFactors = FALSE, row.names = NULL)
    
    for (varNm in df[["Variable"]]) {
      if (varNm %in% names(description))
        df[["Description"]][[which(df[["Variable"]] == varNm)]] <- description[[varNm]]
      if (varNm %in% names(unit))
        df[["Unit"]][[which(df[["Variable"]] == varNm)]] <- unit[[varNm]]
    }
  } else {
    df <- data.frame(Variable = variableNames, Value = values,
                     Unit = character(NROW(ls)),
                     Description = character(NROW(ls)),
                     Interpolation = character(NROW(ls)),
                     stringsAsFactors = FALSE, row.names = NULL)
    
    for (varNm in df[["Variable"]]) {
      if (varNm %in% names(description))
        df[["Description"]][[which(df[["Variable"]] == varNm)]] <- description[[varNm]]
      if (varNm %in% names(unit))
        df[["Unit"]][[which(df[["Variable"]] == varNm)]] <- unit[[varNm]]
      if (varNm %in% names(interpolation))
        df[["Interpolation"]][[which(df[["Variable"]] == varNm)]] <- interpolation[[varNm]]
    }
  }
  
  return(df)
}

# Get function source in string format
FunToString <- function(fun) {
  if(is.null(fun))
    return("")
  funStr <- paste(format(fun), collapse = "\n")
  if(funStr == "NULL")
    return("")
  return(funStr)
}

# Read input as excel file
# 
# First get all the available sheets and then read them all
# 
# @param fileName Excel file name
# @return A list with the sheets as data.frames
ReadDataExcel <- function(fileName)
{
  # read data from excel file with one or more sheets
  sheets <- readxl::excel_sheets(fileName)
  
  modelParms <- lapply(sheets, function(x) 
  {
    tryCatch(
      {
        df <- readxl::read_excel(path = fileName, sheet = x, 
                                 trim_ws = T, col_types = "text", na = " ")
        row.names(df) <- NULL
        df[is.na(df)] <- NULL
        return(as.data.frame(df))
      },
      error=function(e) 
      {
        # readInputDataMsg$ReadDataExcel1(fileName, e)
        return(NULL)
      },
      warning=function(w) 
      {
        # readInputDataMsg$ReadDataExcel2(fileName, w)
        return(NULL)
      })
  })
  names(modelParms) <- sheets
  
  return(modelParms)
}

# Get a value from the same row of a specified variable
GetDataFrameValue <- function(df, variableName, variableCol, valueCol) {
  row <- which(df[[variableCol]] == variableName)
  if(length(row) == 0)
    return(NULL)
  else
    return(df[[valueCol]][[row]])
}
