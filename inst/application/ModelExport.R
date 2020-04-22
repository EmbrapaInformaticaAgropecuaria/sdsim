ModelToXmlHandler <- function(simData, input, output) {
  output$exportModel <- downloadHandler(
    filename = function() {
      tryCatch({
        name <- input$chooseModelName
        
        if(name == "")
          name <- "Model"
        
        if(input$downloadModelAsZip) {
          if(!grepl("\\.zip$", name))
            name <- paste0(name, ".zip")
        } else {
          if(!grepl("\\.xml$", name))
            name <- paste0(name, ".xml")
        }
        
        return(name)
      },
      error = function(e) {
        return("error.txt")
      })
    },
    content = function(file) {
      tryCatch({
        # Updates simData with inputs that have been altered
        UpdateModelData(simData, input)
        
        if(input$downloadModelAsZip) {
          # Get current model
          currentModel <- simData$models[[simData$currentModelId]]
          
          # Create temporary directory to save model and scenarios
          dir <- paste0(tempdir(), "/", currentModel$id)
          if(!dir.exists(dir))
            dir.create(dir)
          
          # Save current working directory
          tempwd <- getwd()
          # Set working directory to temporary directory
          setwd(dir)
          
          # Save model to xml
          modelFile <- paste0(currentModel$id, ".xml")
          ModelToXML(simData, modelFile)
          
          # Get alternative scenario names
          scenarioNames <- names(currentModel$scenarios)
          scenarioNames <- scenarioNames[which(scenarioNames!="Default")]
          
          # Save alternative scenarios to xml
          ziplist <- lapply(scenarioNames, function(x) {
            scenarioFile <- paste0(x, ".xlsx")
            ScenarioToXlsx(currentModel$scenarios[[x]], scenarioFile)
            return(scenarioFile)
          })
          
          # Zip model and scenarios xml
          ziplist <- c(ziplist, modelFile)
          zip(zipfile = file, files = unlist(ziplist))
          
          # Restore working directory
          setwd(tempwd)
          
          # Remove temporary directory
          if(dir.exists(dir)) {
            unlink(dir, recursive = T, force = T)
          }
            
        } else {
          ModelToXML(simData, file)
        }
      },
      error = function(e) {
        showNotification(as.character(e), duration = 15)
      })
    }
  )
}

ScenarioToXmlHandler <- function(simData, input, output) {
  output$exportScenario <- downloadHandler(
    filename = function() {
      tryCatch({
        name <- input$chooseScenarioName
        
        if(name == "")
          name <- "Scenario"
        
        switch (input$scenarioFileType,
                "XML" = {
                  if(!grepl("\\.xml$", name))
                    name <- paste0(name, ".xml")
                },
                "xlsx (excel)" = {
                  if(!grepl("\\.xlsx$", name))
                    name <- paste0(name, ".xlsx")
                }
        )
        
        return(name)
      },
      error = function(e) {
        return("error.txt")
      })
    },
    content = function(file) {
      tryCatch({
        # Updates simData with inputs that have been altered
        UpdateModelData(simData, input)
        
        currentModel <- simData$models[[simData$currentModelId]]
        currentScenario <- currentModel$scenarios[[currentModel$currentScenarioId]]
        switch (input$scenarioFileType,
          "XML" = {
            ScenarioToXML(currentScenario, file)
          },
          "xlsx (excel)" = {
            ScenarioToXlsx(currentScenario, file)
          }
        )
      },
      error = function(e) {
        showNotification(as.character(e), duration = 15)
      })
    }
  )
}

ScenarioToXlsx <- function(scenario, file, colWidth = c(10, 20, 20, 30, 10)) {
  varList <- unlist(lapply(c("method", "from", "to", "by", "id"), function(x) {
    if(!is.null(scenario[[x]]))
      x
    else
      NULL
  }))
  
  valueList <- unlist(lapply(c("method", "from", "to", "by", "id"), function(x) {
    if(!is.null(scenario[[x]]))
      scenario[[x]]
    else
      NULL
  }))
  
  simulationDF <- data.frame(Variable = varList, Value = valueList, 
                             stringsAsFactors = F)
  
  inputData <- list(
    state = scenario$state,
    constant = scenario$constant,
    parameter = scenario$parameter,
    input = scenario$input,
    switch = scenario$switch,
    simulation = simulationDF
  )
  
  # Save to excel
  wb <- openxlsx::createWorkbook()
  lapply(names(inputData), function(x)
  {
    if(!is.null(inputData[[x]])) {
      openxlsx::addWorksheet(wb, sheetName = x)
      openxlsx::writeDataTable(wb = wb, sheet = x, x = inputData[[x]])
      openxlsx::setColWidths(wb = wb, sheet = x, cols = 1:5, 
                             widths = colWidth)
    }
  })
  
  if(!is.null(file))
    openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
}

# Save model to XML
ModelToXML <- function(simData, file = NULL){
  currentModel <- simData$models[[simData$currentModelId]]
  
  if(currentModel$type == "sdOdeModel") {
    modelXml <- OdeModelToXml(currentModel)
  } else if(currentModel$type == "sdStaticModel") {
    modelXml <- StaticModelToXml(currentModel)
  } else if(currentModel$type == "sdCoupledModel") {
    modelXml <- CoupledModelToXml(currentModel, simData)
  }
  
  if (!is.null(file))
    cat(XML::saveXML(modelXml, encoding = "UTF-8",
                     prefix = XmlPrefix(),
                     indent = T),  file = file)
}

# Save ODE model to XML
OdeModelToXml <- function(model) {
  doc = XML::newXMLDoc()
  rootsdModel <- XML::newXMLNode("sdOdeModel", doc = doc)
  
  globalFunctions <- StrGlobalFunctionsToList(model$globalFunctions, 
                                              asCharacter = T)
  
  lModel <- list(id = model$id,
                 description = model$description,
                 # ode = model$ode,
                 InitVars = model$initVars,
                 PostProcessVars = "NULL",
                 RootSpecification = model$root,
                 EventFunction = model$event,
                 aux = DataFrameToList(model$aux),
                 globalFunctions = globalFunctions)
  
  ListToXML(rootsdModel, lModel)
  
  # add the defaultScenario XML
  defaultScenarioId <- model$scenarios[[model$defaultScenarioId]]
  
  sdScenarioXML <- ScenarioToXML(defaultScenarioId)
  XML::xmlName(sdScenarioXML) <- "sdScenario"
  defaultScenarioXML <- XML::newXMLNode("defaultScenario")
  
  odeXML <- XML::newXMLNode("ode",.children = list(odeToXML(model$ode)))

  XML::addChildren(defaultScenarioXML, kids = list(sdScenarioXML))
  XML::addChildren(rootsdModel, kids = list(defaultScenarioXML))
  XML::addChildren(rootsdModel, kids = list(odeXML))
  
  return(doc)
}

odeToXML <- function(ode) {
  rootOde <- XML::newXMLNode("sdFunctionOde")
  lOde <- list(ode = FunToString(ode))
  ListToXML(rootOde, lOde)
  invisible(rootOde)
}

# Save static model to XML
StaticModelToXml <- function(model) {
  doc = XML::newXMLDoc()
  rootsdModel <- XML::newXMLNode("sdStaticModel", doc = doc)
  
  globalFunctions <- StrGlobalFunctionsToList(model$globalFunctions, 
                                              asCharacter = T)
  
  lModel <- list(id = model$id,
                 description = model$description,
                 InitVars = model$initVars,
                 algebraicEquations = DataFrameToList(model$aux),
                 globalFunctions = globalFunctions)
  
  ListToXML(rootsdModel, lModel)
  
  # add the defaultScenario XML
  defaultScenarioId <- model$scenarios[[model$defaultScenarioId]]
  
  sdScenarioXML <- ScenarioToXML(defaultScenarioId)
  XML::xmlName(sdScenarioXML) <- "sdScenario"
  defaultScenarioXML <- XML::newXMLNode("defaultScenario")
  
  XML::addChildren(defaultScenarioXML, kids = list(sdScenarioXML))
  XML::addChildren(rootsdModel, kids = list(defaultScenarioXML))
  return(doc)
}

# Save coupled model to XML
CoupledModelToXml <- function(model, simData) {
  doc = XML::newXMLDoc()
  rootsdModel <- XML::newXMLNode("sdCoupledModel", doc = doc)
  
  # Add the connections
  connections <- unname(split(model$connections, row(model$connections)))
  names(connections) <- model$connections[,1]
  
  lModel <- list(id = model$id,
                 description = model$description,
                 connections = connections)
  
  ListToXML(rootsdModel, lModel)
  
  # add the components
  componentNames <- unique(model$componentIds[,1])
  
  componentsXml <- lapply(componentNames, function(x) {
    component <- simData$models[[x]]
    if(component$type == "sdOdeModel")
      OdeModelToXml(component)
    else if(component$type == "sdStaticModel")
      StaticModelToXml(component)
    else if(component$type == "sdCoupledModel")
      CoupledModelToXml(component, simData)
  })
  
  components <- XML::newXMLNode("components")
  XML::addChildren(components, kids = componentsXml)
  XML::addChildren(rootsdModel, kids = list(components))
  
  return(doc)
}

# Save scenario to XML 
ScenarioToXML <- function(scenario, file = NULL){
  doc = XML::newXMLDoc()
  rootScenario <- XML::newXMLNode("sdScenario", doc = doc)
  
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
  
  if(is.null(scenario$from) && is.null(scenario$to) && is.null(scenario$by))
    times <- NULL
  else
    times <- list(from = scenario$from, to = scenario$to, by = scenario$by)
  
  lscenario <- list(id = scenario$id,
                    times = times,
                    method = scenario$method,
                    state = DataFrameToList(scenario$state),
                    constant = DataFrameToList(scenario$constant),
                    input = DataFrameToList(scenario$input),
                    interpolation = interpolation,
                    parameter = DataFrameToList(scenario$parameter),
                    switch = DataFrameToList(scenario$switch),
                    unit = units,
                    description = descriptions)
  
  ListToXML(rootScenario, lscenario)
  
  if (!is.null(file))
    cat(XML::saveXML(doc, encoding = "UTF-8",
                     prefix = XmlPrefix(),
                     indent = T),  file = file)
  
  return(rootScenario)
}

# source: https://stackoverflow.com/questions/6256064/
# how-to-create-xml-from-r-objects-e-g-is-there-a-listtoxml-function
# Adapted to transform a list of lists in to a XML node tree
ListToXML <- function(node, sublist)
{
  # vectors leafs
  if (is.numeric(sublist) || is.character(sublist)) {
    child <- XML::newXMLNode(names(sublist)[i], parent=node)
    
    if (is.numeric(sublist[[i]]))
      XML::xmlValue(child) <- VectorToCharDef(sublist[[i]]) 
    else # quote
      XML::xmlValue(child) <- VectorToCharDef(sublist[[i]], quote = T) 
    return(node)
  }
  
  # list
  for (i in 1:length(sublist)) {
    child <- XML::newXMLNode(names(sublist)[i], parent=node)
    
    if (typeof(sublist[[i]]) == "list" && length(sublist[[i]]) > 0) {
      ListToXML(child, sublist[[i]])
    } else if (length(sublist[[i]]) > 1) {
      # to store vectors
      if (is.numeric(sublist[[i]]))
        XML::xmlValue(child) <- VectorToCharDef(sublist[[i]]) 
      else # quote
        XML::xmlValue(child) <- VectorToCharDef(sublist[[i]], quote = T) 
    } else {
      XML::xmlValue(child) <- sublist[[i]]
    }
  }
  
  return(node)
}

# Convert vector to source code format (ex: c(1, 2, 3))
VectorToCharDef <- function(x, quote = F) {
  if (quote)
    return(paste0("c(", paste0("'", x, "'", collapse = ","), ")"))
  else
    return(paste0("c(", paste0(x, collapse = ","), ")"))
}

# return the sdsim package XML prefix
XmlPrefix <- function() {
  return(paste0("<?sdsim about='R package for ",
                "modeling and simulation of system dynamics'",
                " version='",
                toString(utils::packageVersion("sdsim")),
                "' date='", Sys.Date(),
                "'?>"))
}
