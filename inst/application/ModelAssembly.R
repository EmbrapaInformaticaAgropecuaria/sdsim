# Assemble model object using the functions defined in the script areas
AssembleModel <- function(simData, input, timeSeriesDirectory, 
                          progressFunction = NULL) {
  # Updates simData with inputs that have been altered
  UpdateModelData(simData, input)
  
  model <- simData$models[[simData$currentModelId]]
  
  if(model$type == "sdAtomicModel") {
    modelObj <- AssembleAtomicModel(model, timeSeriesDirectory, progressFunction)
    return(modelObj)
  } else if (model$type == "sdStaticModel") {
    modelObj <- AssembleStaticModel(model, timeSeriesDirectory)
    return(modelObj)
  } else if (model$type == "sdCoupledModel") {
    modelObj <- AssembleCoupledModel(model, simData, timeSeriesDirectory)

    modelObj$buildCoupledModel(from = as.numeric(input$initialTime),
                               to = as.numeric(input$finalTime),
                               by = as.numeric(input$step),
                               method = input$method,
                               timeSeriesDirectory = timeSeriesDirectory)
    return(modelObj)
  }
}

AssembleCoupledModel <- function(model, simData, timeSeriesDirectory) {
  componentNames <- unique(model$componentIds[,1])
  
  componentsList <- lapply(componentNames, function(x) {
    component <- simData$models[[x]]
    if(component$type == "sdAtomicModel")
      AssembleAtomicModel(component, timeSeriesDirectory)
    else if(component$type == "sdStaticModel")
      AssembleStaticModel(component, timeSeriesDirectory)
    else if(component$type == "sdCoupledModel")
      AssembleCoupledModel(component, simData, timeSeriesDirectory)
  })
  
  connections <- unname(split(model$connections, row(model$connections)))
  
  modelObj <- sdsim::sdCoupledModel(
    id = model$id,
    components = componentsList,
    connections = connections,
    description = model$description)
  return(modelObj)
}

AssembleAtomicModel <- function(model, timeSeriesDirectory, progressFunction = NULL) {
  defaultScenario <- model$scenarios[[model$defaultScenarioId]]
  
  DifferentialEquationsStr <- model$DifferentialEquations
  
  # Insert function to update progress into the function code using regex
  if(!is.null(progressFunction)) {
    DifferentialEquationsStr <- sub("^(.*function.*\\(.*\\)(.|\n)*\\{)((.|\n|\t)*)$",
                                    "\\1\n  UpdateSimulationProgress(t)\\3",
                                    DifferentialEquationsStr)
  }
  
  # Assemble ode function
  DifferentialEquations <- eval(parse(text = DifferentialEquationsStr))

  # If there is an initialization function parse it
  if(!is.null(model$initVars) &&
     !grepl(EMPTY_PERL_REGEX, model$initVars, perl = T))
    InitVars <- eval(parse(text = model$initVars)) 
  else 
    InitVars <- NULL
  
  # If There is a root function script parse it
  if(!is.null(model$root) &&
     !grepl(EMPTY_PERL_REGEX, model$root, perl = T))
    RootFunction <- eval(parse(text = model$root)) 
  else 
    RootFunction <- NULL
  
  # If there is an event function script parse it
  if(!is.null(model$event) &&
     !grepl(EMPTY_PERL_REGEX, model$event, perl = T))
    EventFunction <- eval(parse(text = paste(model$event))) 
  else 
    EventFunction <- NULL
  
  # Parse global functions into a list
  if(!is.null(model$globalFunctions))
    globalFunctions <- StrVariablesToList(model$globalFunctions)
  
  # Set function environments to global to reduce object size and
  # remove server variables from the object
  if(!is.null(DifferentialEquations))
    environment(DifferentialEquations) <- globalenv()
  if(!is.null(InitVars))
    environment(InitVars) <- globalenv()
  if(!is.null(RootFunction))
    environment(RootFunction) <- globalenv()
  if(!is.null(EventFunction))
    environment(EventFunction) <- globalenv()
  
  auxList <- DataFrameToList(model$aux, convertType = F)
  if(length(auxList) == 0)
    auxList <- NULL
  
  auxUnits <- DataFrameToList(model[["aux"]], valueCol = "Unit")
  auxUnits <- auxUnits[which(auxUnits != "")]
  
  auxDescriptions <- DataFrameToList(model[["aux"]], valueCol = "Description")
  auxDescriptions <- auxDescriptions[which(auxDescriptions != "")]
  
  defaultScenarioObj <- sdsim::sdScenario(
    id = defaultScenario$id,
    times = list(from = defaultScenario$from,
                 to = defaultScenario$to,
                 by = defaultScenario$by),
    method = defaultScenario$method,
    state = NullIfEmptyDF(defaultScenario$state),
    constant = NullIfEmptyDF(defaultScenario$constant),
    input = NullIfEmptyDF(defaultScenario$input),
    parameter = NullIfEmptyDF(defaultScenario$parameter),
    switch =  NullIfEmptyDF(defaultScenario$switch),
    timeSeriesDirectory = timeSeriesDirectory,
    unit = auxUnits,
    description = auxDescriptions)
  
  modelObj <- sdsim::sdAtomicModel(
    id = model$id,
    DifferentialEquations = DifferentialEquations,
    defaultScenario = defaultScenarioObj,
    InitVars = InitVars,
    RootSpecification = RootFunction,
    EventFunction = EventFunction,
    description = model$description,
    aux = auxList,
    globalFunctions = globalFunctions)
  
  if(!is.null(progressFunction))
    assign("UpdateSimulationProgress", progressFunction, 
           envir = environment(modelObj$DifferentialEquations))
  
  return(modelObj)
}

AssembleStaticModel <- function(model, timeSeriesDirectory) {
  defaultScenario <- model$scenarios[[model$defaultScenarioId]]
  
  # If there is an initialization function parse it
  if(!is.null(model$initVars) &&
     !grepl(EMPTY_PERL_REGEX, model$initVars, perl = T))
    InitVars <- eval(parse(text = model$initVars)) 
  else 
    InitVars <- NULL
  
  # Parse global functions into a list
  if(!is.null(model$globalFunctions))
    globalFunctions <- StrVariablesToList(model$globalFunctions)
  
  # Set function environments to global to reduce object size and
  # remove server variables from the object
  if(!is.null(InitVars))
    environment(InitVars) <- globalenv()
  
  auxList <- DataFrameToList(model$aux, convertType = F)
  if(length(auxList) == 0)
    auxList <- NULL
  
  auxUnits <- DataFrameToList(model[["aux"]], valueCol = "Unit")
  auxUnits <- auxUnits[which(auxUnits != "")]
  
  auxDescriptions <- DataFrameToList(model[["aux"]], valueCol = "Description")
  auxDescriptions <- auxDescriptions[which(auxDescriptions != "")]
  
  defaultScenarioObj <- sdsim::sdScenario(
    id = defaultScenario$id,
    times = list(from = defaultScenario$from,
                 to = defaultScenario$to,
                 by = defaultScenario$by),
    state = NullIfEmptyDF(defaultScenario$state),
    constant = NullIfEmptyDF(defaultScenario$constant),
    input = NullIfEmptyDF(defaultScenario$input),
    parameter = NullIfEmptyDF(defaultScenario$parameter),
    switch =  NullIfEmptyDF(defaultScenario$switch),
    timeSeriesDirectory = timeSeriesDirectory,
    unit = auxUnits,
    description = auxDescriptions)
  
  
  modelObj <- sdsim::sdStaticModel(
    id = model$id,
    description = model$description,
    defaultScenario = defaultScenarioObj,
    equations = auxList, 
    InitVars = InitVars,
    globalFunctions = globalFunctions)
  
  return(modelObj)
}

AssembleAlternateScenario <- function(simData, timeSeriesDirectory) {
  model <- simData$models[[simData$currentModelId]]
  
  if((!is.null(model$defaultScenarioId) &&
      model$defaultScenarioId != model$currentScenarioId) ||
     (is.null(model$defaultScenarioId) && 
      !is.null(model$currentScenarioId)))
    currentScenario <- model$scenarios[[model$currentScenarioId]]
  else
    currentScenario <- NULL
  
  if(!is.null(currentScenario))
    currentScenarioObj <- sdsim::sdScenario(
      id = currentScenario$id,
      times = list(from = currentScenario$from,
                   to = currentScenario$to,
                   by = currentScenario$by),
      method = currentScenario$method,
      state = NullIfEmptyDF(currentScenario$state),
      constant = NullIfEmptyDF(currentScenario$constant),
      input = NullIfEmptyDF(currentScenario$input),
      parameter = NullIfEmptyDF(currentScenario$parameter),
      switch =  NullIfEmptyDF(currentScenario$switch),
      timeSeriesDirectory = timeSeriesDirectory)
  else
    currentScenarioObj <- NULL
  
  return(currentScenarioObj)
}

# Update simData reactive list if the model has been altered
UpdateModelData <- function(simData, input) {
  currentModel <- simData$models[[simData$currentModelId]]

  # if current scenario is null set the first model scenario as current scenario
  if(is.null(currentModel$currentScenarioId)){
    currentModel$currentScenarioId <- currentModel$scenarios[[1]]$id
  }
      
  currentScenario <- currentModel$scenarios[[currentModel$currentScenarioId]]
  
  if(currentModel$type == "sdAtomicModel") {
    # Update current model description, functions and auxiliaries
    currentModel$description <- input$description
    currentModel$DifferentialEquations <- input$DifferentialEquations
    currentModel$initVars <- input$initVars
    currentModel$root <- input$root
    currentModel$event <- input$event
    currentModel$globalFunctions <- input$globalFunctions
    
    if(!is.null(simData$changed$aux) && simData$changed$aux) {
      currentModel$aux <- RhandsonToDF(input$aux, trimWhites = NULL)
    }
  } else if (currentModel$type == "sdStaticModel") {
    currentModel$description <- input$description
    currentModel$initVars <- input$initVars
    currentModel$globalFunctions <- input$globalFunctions
    
    if(!is.null(simData$changed$staticAux) && simData$changed$staticAux) {
      currentModel$aux <- RhandsonToDF(input$staticAux, trimWhites = NULL)
    }
  } else if(currentModel$type == "sdCoupledModel") {
    if(!is.null(simData$changed$connections) && simData$changed$connections) {
      currentModel$connections <- as.matrix(
        RhandsonToDF(
          input$connections, trimWhites = NULL, variableCol = "Connection ID")
      )
    } 
    
    if(!is.null(simData$changed$componentIds) && simData$changed$componentIds) {
      currentModel$componentIds <- as.matrix(
        RhandsonToDF(
          input$componentIds, trimWhites = NULL, variableCol = "Component ID")
      )
    } 
  }
  
  # Update method, except for static model scenarios
  if(currentModel$type != "sdStaticModel") {
    # Update current scenario
    currentScenario$method <- input$method
  }
  
  # Update times
  currentScenario$from <- input$initialTime
  currentScenario$to <- input$finalTime
  currentScenario$by <- input$step
  
  if(!is.null(simData$changed$state) && simData$changed$state) {
    currentScenario$state <- RhandsonToDF(input$state, trimWhites = c("Variable", "Value"))
  }
  
  if(!is.null(simData$changed$constant) && simData$changed$constant) {
    currentScenario$constant <- RhandsonToDF(input$constant, trimWhites = c("Variable", "Value"))
  }
  
  if(!is.null(simData$changed$input) && simData$changed$input) {
    currentScenario$input <- RhandsonToDF(input$input, trimWhites = c("Variable", "Value"))
  }
  
  if(!is.null(simData$changed$parameter) && simData$changed$parameter) {
    currentScenario$parameter <- RhandsonToDF(input$parameter, trimWhites = c("Variable", "Value"))
  }
  
  if(!is.null(simData$changed$switch) && simData$changed$switch) {
    currentScenario$switch <- RhandsonToDF(input$switch, trimWhites = c("Variable", "Value"))
  }
  
  currentModel$scenarios[[currentModel$currentScenario]] <- currentScenario
  simData$models[[simData$currentModelId]] <- currentModel
}

# Transforms a rhandsontable object in a data frame object removing empty lines
RhandsonToDF <- function(hot, trimWhites = NULL, variableCol = "Variable") {
  if(is.null(hot))
    return(NULL)
  
  df <- rhandsontable::hot_to_r(hot)
  
  if(is.null(df))
    return(NULL)

  for(col in trimWhites) {
    df[col] <- as.data.frame(apply(df[col], c(1, 2), function(x) gsub('\\s+', '',x)), stringsAsFactors = F)
  }
  
  df <- df[-which(grepl(EMPTY_PERL_REGEX, df[[variableCol]], perl = T)), , drop = FALSE]
  row.names(df) <- NULL
  
  return(df)
}

# Convert a data.frame to a list 
DataFrameToList <- function(dataFrame, variableCol = "Variable", 
                            valueCol = "Value", dec = ".",
                            convertType = F) {
  dataList <- list()
  # if file is not empty
  if (!is.null(dataFrame) && nrow(dataFrame) > 0) {
    # convert to list
    dataList <- as.list(unlist(apply(dataFrame, 1, function(x) {
      l <- list()
      
      if (is.character(x[[valueCol]]) && convertType)
        l[x[[variableCol]]] <- type.convert(x[[valueCol]], dec = dec, 
                                            numerals = "warn.loss", as.is = TRUE) 
      else 
        l[x[[variableCol]]] <- x[[valueCol]]
      
      
      return(l)
    }), recursive = F)) 
  }
  
  return(dataList)
}

StrVariablesToList <- function(strFuns) {
  tempEnvironment <- new.env()
  eval(parse(text = strFuns), envir = tempEnvironment)
  
  funNames <- all.vars(parse(text = strFuns))
  
  l <- lapply(funNames, function(x) {
    return(get(x, envir = tempEnvironment))
  })
  names(l) <- funNames
  return(l)
}

# Returns NULL if a data frame is empty
NullIfEmptyDF <- function(df) {
  if(NROW(df) > 0)
    df
  else
    NULL
}