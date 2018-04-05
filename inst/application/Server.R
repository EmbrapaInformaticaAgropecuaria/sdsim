# shinyServer

source("RegularExpressions.R", local = TRUE)
source("DataHandler.R", local = TRUE)
source("ModelAssembly.R", local = TRUE)
source("OutputHandler.R", local = TRUE)
source("ModelExport.R", local = TRUE)
source("ModelImport.R", local = TRUE)

server <- shinyServer(function(input, output, session) { 
  nTableRows <- 50
  
  # Reactive list containing the models, scenarios and simulation results
  simData <- reactiveValues()
  simData$models <- list()
  simData$changed <- list()
  
  # Use a temporary directory for saving time series files
  timeSeriesDirectory <- tempdir()
  
  isolate(LoadModel("UnnamedModel", simData, session, input, output, "application/xml", nTableRows = nTableRows))
  
  output$selectModelOutput <- renderUI({
    choices <- names(simData$models)
    if(length(choices) > 0){
      selected <- simData$currentModelId
      
      if(length(choices) == 0) {
        choices <- "Unnamed model"
        selected <- "Unnamed model"
      }
      
      selectInput("selectModel", "Model", choices = choices, selected = selected)
    } else {
      NULL
    }
  })
  
  output$selectScenarioOutput <- renderUI({
    currentModel <- NULL
    choices <- NULL
    
    if(length(simData$models) > 0) {
      currentModel <- simData$models[[simData$currentModelId]]
    }

    if(!is.null(currentModel) && length(currentModel$scenarios) > 0) {
      choices <- names(currentModel$scenarios)
    }
    
    if(!is.null(choices)) {
      selected <- currentModel$currentScenarioId
      if(!is.null(selected))
        selectInput("selectScenario", "Scenario", choices = choices, selected = selected)
      else
        selectInput("selectScenario", "Scenario", choices = choices)
    } else {
      NULL
    }
  })
  
  observeEvent(input$selectModel, {
    # Update simData changes if switching models
    # Observer is also activated when a model is loaded, but should not update
    # When a model is loaded simData$currentModelId is equal to input$selectModel
    # When switching using the select input, both are different
    if(simData$currentModelId != input$selectModel)
      UpdateModelData(simData, input)
    
    # Update current model
    simData$currentModelId <- input$selectModel
    # Update UI
    UpdateLoadedModel(simData, session, input, output, nTableRows)
    UpdateLoadedScenario(simData, session, input, output, nTableRows)
    
    UpdatePlotInput(simData, session)
  })
  
  observeEvent(input$selectScenario, {
    if(!is.null(simData$models[[simData$currentModelId]]$currentScenarioId) &&
      simData$models[[simData$currentModelId]]$currentScenarioId != input$selectScenario)
      UpdateModelData(simData, input)
    
    # Update current scenario
    simData$models[[simData$currentModelId]]$currentScenarioId <- input$selectScenario
    
    if(input$selectScenario == "Default")
      session$sendCustomMessage("hideElement", "editScenarioIdDiv")
    else
      session$sendCustomMessage("unhideElement", "editScenarioIdDiv")
    
    # Update UI
    UpdateLoadedScenario(simData, session, input, output, nTableRows)
  })
  # Reactive list containing the necessary data for running a simulation
  modelData <- reactiveValues()
  
  # Load new scenario
  observeEvent(input$newScenario, {
    showModal(modalDialog(
      id = "newScenarioModal",
      title = "New Scenario",
      easyClose = TRUE,
      size = "m",
      
      h4(strong("Create Empty scenario")),
      div(
        textInput("scenarioIdInput", "Choose the scenario ID:", placeholder = "Unnamed Scenario"),
        style = "display: inline-block;vertical-align:top; padding: 0px 10px 15px 0px;"
      ),
      div(
        actionButton("newEmptyScenario", strong("Create Scenario"),
                     width = "120px"),
        style = "display: inline-block;vertical-align:top; padding: 25px 10px 15px 0px;"
      ),
      div(style = "border: 1px solid grey"),
      h4(strong("Import scenario from file")),
      fileInput("importScenario", "Choose the scenario file", accept = c(".xml", ".xlsx")),
      
      div(
        textOutput("loadedScenarioMessage")
      ),
      
      style = "
      padding-left: 10px;
      padding-right: 10px;
      word-wrap: break-word;"
    ))
  })
  
  observeEvent(input$newModel, {
    showModal(modalDialog(
      id = "newModelModal",
      title = "New Model",
      easyClose = TRUE,
      size = "m",
      
      h4(strong("Create Empty model")),
      radioButtons("newModelType", "Model type", 
                   choices = c("Atomic", "Static", "Coupled"),
                   selected = "Atomic",
                   inline = T),
      div(
        textInput("modelIdInput", "Choose the model ID:", placeholder = "Unnamed Model"),
        style = "display: inline-block;vertical-align:top; padding: 0px 10px 15px 0px;"
      ),
      div(
        actionButton("newEmptyModel", strong("Create Model"),
                     width = "120px"),
        style = "display: inline-block;vertical-align:top; padding: 25px 10px 15px 0px;"
      ),
      div(style = "border: 1px solid grey"),
      h4(strong("Import model from file")),
      fileInput("importModel", "Choose the model file", accept = c(".xml")),
      div(style = "border: 1px solid grey"),
      h4(strong("Clone Current Model")),
      div(
        textInput("cloneModelIdInput", "Choose the cloned model ID:", placeholder = "Unnamed Model"),
        style = "display: inline-block;vertical-align:top; padding: 0px 10px 15px 0px;"
      ),
      div(
        actionButton("newCloneModel", strong("Clone Model"),
                     width = "120px"),
        style = "display: inline-block;vertical-align:top; padding: 25px 10px 15px 0px;"
      ),
      div(style = "border: 1px solid grey"),
      h4(strong("Examples")),
      div(
        selectInput("selectExample", "Choose a Model", 
                    choices = c("Arenstorf", "BouncingBall", 
                                "Customer", "RigidBody"),
                    width = "200"),
        style = "display: inline-block;vertical-align:top; padding: 0px 10px 15px 0px;"
      ),
      div(
        actionButton("loadExample", strong("Load Example"),
                     width = "120px"),
        style = "display: inline-block;vertical-align:top; padding: 25px 10px 15px 0px;"
      ),
      
      div(
        textOutput("loadedModelMessage")
      ),
      style = "
      padding-left: 10px;
      padding-right: 10px;
      word-wrap: break-word;"
    ))
  })
  
  observeEvent(input$newCloneModel, {
    modelName <- input$cloneModelIdInput
    
    if(grepl("^[ \t\n]$", modelName))
      modelName <- "Unnamed Model"
    
    if(modelName %in% names(simData$models)) {
      # Save loading model info in simData
      simData$cloningModelName <- modelName
      
      idsToOverwrite <- modelName
      message <- paste0("The model \"", idsToOverwrite, "\" will be overwritten.",
                        "\nDo you wish to continue?")
      responseInputName <- "confirmCloneModelOverwrite"
      session$sendCustomMessage("confirmOverwrite", 
                                list(message = message, 
                                     responseInputName = responseInputName))
    } else {
      simData$models[[modelName]] <- simData$models[[simData$currentModelId]]
      simData$currentModelId <- modelName
      
      msg <- list(
        paste(modelName, "model successfully cloned!"),
        "green"
      )
      
      session$sendCustomMessage("loadedModelMessage", msg)
    }
  })
  
  # load model if overwrite confirmation is received
  observeEvent(input$confirmCloneModelOverwrite, {
    if(input$confirmCloneModelOverwrite[[1]] == 1) {
      modelName <- simData$cloningModelName
      simData$cloningModelName <- NULL
      
      simData$models[[modelName]] <- simData$models[[simData$currentModelId]]
      simData$currentModelId <- modelName
      
      msg <- list(
        paste(modelName, "model successfully cloned!"),
        "green"
      )
      session$sendCustomMessage("loadedModelMessage", msg)
    }
  })
  
  # load model if overwrite confirmation is received
  observeEvent(input$confirmModelOverwrite, {
    if(input$confirmModelOverwrite[[1]] == 1) {
      msg <- ConfirmLoadModel(simData$loadingModel, simData, session, input, 
                       output, nTableRows = nTableRows)
      simData$loadingModel <- NULL
      session$sendCustomMessage("loadedModelMessage", msg)
    }
  })
  
  # load scenario if overwrite confirmation is received
  observeEvent(input$confirmScenarioOverwrite, {
    if(input$confirmScenarioOverwrite[[1]] == 1) {
      msg <- ConfirmLoadScenario(simData$loadingScenario, simData, session, input,
                              output, nTableRows = nTableRows)
      simData$loadingScenario <- NULL
      session$sendCustomMessage("loadedScenarioMessage", msg)
    }
  })
  
  # Create an empty model
  observeEvent(input$newEmptyModel, {
    # Update previous model data
    UpdateModelData(simData, input)
    
    switch (
      input$newModelType,
      "Atomic" = {
        # Load empty model and replace ID
        msg <- LoadModel("UnnamedModel", simData, session, input, output, "application/xml", 
                         replaceId = input$modelIdInput, nTableRows = nTableRows)
      }, "Static" = {
        msg <- LoadModel("UnnamedStaticModel", simData, session, input, output, "application/xml", 
                         replaceId = input$modelIdInput, nTableRows = nTableRows)
      }, "Coupled" = {
        msg <- LoadModel("UnnamedCoupledModel", simData, session, input, output, "application/xml", 
                         replaceId = input$modelIdInput, nTableRows = nTableRows)
      }
    )
    
    session$sendCustomMessage("loadedModelMessage", msg)
  })
  
  # Delete current model
  observeEvent(input$deleteModel, {
    selected <- simData$currentModelId
    simData$models[[selected]] <- NULL
    
    modelIds <- names(simData$models)
    if(length(modelIds) > 0) {
      simData$currentModelId <- modelIds[[1]]
    } else {
      # If the model list is empty load an empty model
      LoadModel("UnnamedModel", simData, session, input, output, "application/xml", nTableRows = nTableRows)
    }
  })
  
  # Delete current model
  observeEvent(input$deleteScenario, {
    currentModel <- simData$models[[simData$currentModelId]]
    selected <- currentModel$currentScenarioId
    
    simData$models[[simData$currentModelId]]$scenarios[[selected]] <- NULL
    scenarioIds <- names(simData$models[[simData$currentModelId]]$scenarios)
    
    if(length(scenarioIds) > 0) {
      simData$models[[simData$currentModelId]]$currentScenarioId <- scenarioIds[[1]]
    } else {
      # If the scenario list is empty load an empty scenario
      LoadScenario("UnnamedScenario", simData, session, input, output, "application/xml", nTableRows = nTableRows)
    }
  })
  
  # Change model ID modal
  observeEvent(input$editModelId, {
    showModal(modalDialog(
      id = "editModelModal",
      title = "Change Model ID",
      easyClose = TRUE,
      size = "s",
      h5("This will change your current model ID."),
      textInput("editModelIdTxt", "Choose an ID", value = simData$currentModelId),
      actionButton("updateModelId", "Update Model ID", icon = icon("edit")),
      div(
        textOutput("updatedModelIdMessage")
      )
    ))
  })
  
  # Change scenario ID modal
  observeEvent(input$editScenarioId, {
    showModal(modalDialog(
      id = "editScenarioModal",
      title = "Change Scenario ID",
      easyClose = TRUE,
      size = "s",
      h5("This will change your scenario model ID."),
      textInput("editScenarioIdTxt", "Choose an ID", 
                value = simData$models[[simData$currentModelId]]$currentScenarioId),
      actionButton("updateScenarioId", "Update Scenario ID", icon = icon("edit")),
      div(
        textOutput("updatedScenarioIdMessage")
      )
    ))
  })
  
  # Update model ID button
  observeEvent(input$updateModelId, {
    if(grepl("^[ \t\n]*$", input$editModelIdTxt)){
      msg <- "Please, choose a new ID for the model"
      session$sendCustomMessage("updatedModelIdMessage", 
                                list(msg, "red"))
    } else{
      if(input$editModelIdTxt %in% names(simData$models)){
        msg <- paste0("There is already a loaded model with ID ", 
                      input$editModelIdTxt, 
                      ". Please, choose a different ID.")
        session$sendCustomMessage("updatedModelIdMessage", list(msg, "red"))
      } else {
        currentModelId <- simData$currentModelId
        names(simData$models) <- gsub(currentModelId, 
                                      input$editModelIdTxt, 
                                      names(simData$models))
        simData$currentModelId <- input$editModelIdTxt
        msg <- paste0("Current model ID changed from ", currentModelId,
                      " to ", input$editModelIdTxt)
        session$sendCustomMessage("updatedModelIdMessage", list(msg, "green"))
      }
    }
  })
  
  # Update scenario ID button
  observeEvent(input$updateScenarioId, {
    if(grepl("^[ \t\n]*$", input$editScenarioIdTxt)){
      msg <- "Please, choose a new ID for the model"
      session$sendCustomMessage("updatedScenarioIdMessage", 
                                list(msg, "red"))
    } else{
      currentModel <- simData$models[[simData$currentModelId]]
      
      if(input$editScenarioIdTxt %in% names(currentModel$scenarios)){
        msg <- paste0("There is already a scenario model with ID ", 
                      input$editScenarioIdTxt, 
                      ". Please, choose a different ID.")
        session$sendCustomMessage("updatedScenarioIdMessage", list(msg, "red"))
      } else {
        currentScenarioId <- currentModel$currentScenarioId
        names(simData$models[[simData$currentModelId]]$scenarios) <- gsub(currentScenarioId, 
                                                                input$editScenarioIdTxt, 
                                                                names(currentModel$scenarios))
        
        simData$models[[simData$currentModelId]]$currentScenarioId <- input$editScenarioIdTxt
        msg <- paste0("Current model ID changed from ", currentScenarioId,
                      " to ", input$editScenarioIdTxt)
        session$sendCustomMessage("updatedScenarioIdMessage", list(msg, "green"))
      }
    }
  })
  
  # Update method select input based on whether a root function is available
  ObserveRootMethod(input, session)
  
  # Check if any changes were made to variables since the last model upload
  ObserveRhandsonChanges(simData, input)
  
  # Force refresh ace editor script areas if a file has been uploaded (ace editor bug)
  ObserveScriptChanges(input, session)
  
  # Simulation name. Changes when a model is loaded.
  # output$modelId <- renderText(modelData$modelId)
  
  # Observe if a model XML file is uploaded
  observeEvent(input$importModel, {
    if(!is.null(input$importModel)) {
      # If file is XML
      if(grepl(".[xX][mM][lL]$", input$importModel$name)) {
        # Update previous model data
        UpdateModelData(simData, input)
        
        msg <- LoadModel(input$importModel$datapath, simData, session, input, 
                         output, nTableRows = nTableRows)
        
        session$sendCustomMessage("loadedModelMessage", msg)
      }
    }
  })
  
  # Create scenario from XML or XLSX file
  observeEvent(input$importScenario, {
    if(!is.null(input$importScenario)) {
      # If file is XML
      if(grepl(".[xX][mM][lL]$|.[xX][lL][sS][xX]$", input$importScenario$name)) {
        # Update previous model data
        UpdateModelData(simData, input)
        
        msg <- LoadScenario(input$importScenario$datapath, simData, session, input, 
                            output, nTableRows = nTableRows)
        session$sendCustomMessage("loadedScenarioMessage", msg)
      } else {
        msg <- list(
          "<simpleError: Load model aborted. The given file is not a valid 
          scenario file. Only xlsx and xml formats are accepted.>",
          "green")
        session$sendCustomMessage("loadedScenarioMessage", msg)
      }
    }
  })
  
  # Create empty scenario
  observeEvent(input$newEmptyScenario, {
    # Update previous model data
    UpdateModelData(simData, input)
    
    msg <- LoadScenario("UnnamedScenario", simData, session, input, 
                        output, "application/xml", nTableRows = nTableRows,
                        replaceId = input$scenarioIdInput)
    session$sendCustomMessage("loadedScenarioMessage", msg)
  })
  
  # Load Examples
  observeEvent(input$loadExample, {
    # Update previous model data
    UpdateModelData(simData, input)
    
    msg <- LoadModel(input$selectExample, simData, session, input, output, "repository", nTableRows = nTableRows)
    session$sendCustomMessage("loadedModelMessage", msg)
  })
  
  # Observe if time series files are uploaded
  observeEvent(input$importTimeSeries, {
    inFile <- input$importTimeSeries
    if(!is.null(inFile)) {
      if(!dir.exists(timeSeriesDirectory))
        dir.create(timeSeriesDirectory)
      
      apply(inFile, 1, function(f) {
        withCallingHandlers({
          tryCatch({
            f <- as.list(f)
            
            read.table(file = f$datapath, 
                       colClasses = c("numeric", "numeric"), 
                       dec = ".", sep = ",", 
                       strip.white = TRUE, 
                       stringsAsFactors = FALSE,
                       header = TRUE)
            
            modelData$timeSeriesFiles <- unique(c(modelData$timeSeriesFiles, f$name))
            file.copy(f$datapath, file.path(timeSeriesDirectory, f$name), overwrite = T)
          },
          error = function(e) {
            errorOutput <- paste0(f$name, ": File format not recognized. ",
                                  "Ensure the file has headers ", 
                                  "and that the values are separated by comma.")
            showNotification(errorOutput, duration = 15)
            session$sendCustomMessage("shinyWarning", errorOutput)
          })
        },
        warning = function(w) {
          # warningOutput <- paste0(f$name, ": File format not recognized. ",
          #                         "Ensure the file has headers ", 
          #                         "and that the values are separated by comma.")
          # showNotification(warningOutput, duration = 15)
          # session$sendCustomMessage("shinyWarning", warningOutput)
        })
      })
    }
  })
  
  # Displays modal to view the uploaded time series
  output$timeSeriesSelectedFile <- renderText(input$selectTs)
  observeEvent(input$viewTimeSeries, {
    # Keep previously selected radio button when reopening the modal
    selectedTs <- input$selectTs
    if(is.null(selectedTs))
      selectedTs <- character(0)
    
    if(length(modelData$timeSeriesFiles) > 0) {
      timeSeriesFiles <- 
        radioButtons("selectTs", NULL, 
                     choices = modelData$timeSeriesFiles, 
                     width = "100%", selected = selectedTs)
    } else {
      timeSeriesFiles <- 
        radioButtons("selectTs", NULL, 
                     choices = "No files were uploaded", 
                     width = "100%", selected = "No files were uploaded")
    }
    
    showModal(modalDialog(
      title = "Uploaded Time Series",
      easyClose = TRUE,
      size = "m",
      div(
        timeSeriesFiles,
        style = "max-height: 200px;
        overflow-y: auto;"
      ),
      conditionalPanel("input.selectTs != null && input.selectTs != 'No files were uploaded'",
                       br(),
                       div(style = "border: 1px solid grey"),
                       br(),
                       span(textOutput("timeSeriesSelectedFile"), 
                            style = "font-style: bold; font-size: 20px; 
                            paddng-left: 10px; text-align: center;"),
                       shinydashboard::tabBox(
                         title = "", 
                         width = "100%",
                         tabPanel("Data", dataTableOutput("timeSeriesTable")),
                         tabPanel("Plot",
                                  radioButtons("TsPlotType", NULL, 
                                               choices = c("line", "point"), 
                                               width = "100%", 
                                               selected = "line", 
                                               inline = T),
                                  plotOutput("timeSeriesPlot" ))
                       )
      ),
      style = "
      padding-left: 10px;
      padding-right: 10px;
      word-wrap: break-word;"
    ))
  })
  
  observeEvent(input$savePlot, {
    # Keep previous width and height values
    plotWidth <- input$plotWidth
    if(is.null(plotWidth))
      plotWidth <- 1366
    
    plotHeight <- input$plotHeight
    if(is.null(plotHeight))
      plotHeight <- 768
    
    showModal(modalDialog(
      title = "Save Plot",
      size = "s",
      easyClose = TRUE,
      div(
        numericInput(inputId = "plotWidth", 
                     label = "Width", 
                     value = plotWidth,
                     min = 500,
                     max = 2000),
        style = "display: inline-block;vertical-align:top; width: 80px; padding: 0px 0px 0px 0px;"
      ),
      div(
        h5("x"),
        style = "display: inline-block;vertical-align:top; width: 10px; padding: 23px 10px 0px 4px;"
      ),
      div(
        numericInput(inputId = "plotHeight", 
                     label = "Height", 
                     value = plotHeight,
                     min = 500,
                     max = 2000),
        style = "display: inline-block;vertical-align:top; width: 80px; padding: 0px 0px 0px 0px;"
      ),
      div(
        h5("pixels"),
        style = "display: inline-block;vertical-align:top; width: 10px; padding: 23px 10px 0px 4px;"
      ),
      downloadButton("savePlotDl"),
      # Limit plot width and height values
      tags$script(HTML('plotWidth.onchange = function() {
                                       if(this.value < 500) {
                                         this.value = 500;
                                       } else if(this.value > 2000) {
                                          this.value = 2000;
                                       }
                                     }

                                    plotHeight.onchange = function() {
                                       if(this.value < 500) {
                                         this.value = 500;
                                       } else if(this.value > 2000) {
                                         this.value = 2000;
                                       }
                                    }
                                    '
      ))
    ))
  })
  
  output$savePlotDl <- downloadHandler(
    filename = function(){
      currentModel <- simData$models[[simData$currentModelId]]
      currentScenario <- currentModel$scenarios[[currentModel$currentScenarioId]]
      
      paste0(currentModel$modelId, "(", currentScenario$id, ")_plot.png")
    }, 
    content = function(file) {
      tryCatch({
        out <- simData$models[[simData$currentModelId]]$out
        # out <- simData$out
        if(!is.null(out) && length(input$selVarPlot) > 0) {
          variables <- paste(input$selVarPlot, collapse = " ")
          variables <- paste(variables, "~" ,input$selectXAxisPlot)
          
          if(!is.null(input$plotTitle) && input$plotTitle != "")
            main <- input$plotTitle 
          else 
            main <- NULL
          
          if(!is.null(input$plotXLabel) && input$plotXLabel != "")
            xlab <- input$plotXLabel 
          else 
            xlab <- NULL
          
          if(!is.null(input$plotYLabel) && input$plotYLabel != "")
            ylab <- strsplit(input$plotYLabel, ",") 
          else 
            ylab <- NULL
          
          type <- switch(input$plotType, "line" = "l", "point" = "p")
          
          png(file,
              width = input$plotWidth,
              height = input$plotHeight)
          
          out$plot(variables, xlab = xlab, ylab = ylab, multipleYAxis = input$multipleAxisToggle, main = main, type = type)
          
          dev.off()
        }
      }, error = function(e) {
      })
    }
  )
  
  # Modal dialog window to save a model 
  observeEvent(input$saveModel, ShowModelDownloadDialog(simData))
  observeEvent(input$shortcut, ShowModelDownloadDialog(simData))
  observeEvent(input$saveScenario, ShowScenarioDownloadDialog(simData))
  
  # Render simulation results
  RenderDataTables(simData, output)
  RenderDownloadButtons(simData, output)
  TrajectoriesDownloadHandler(simData, output)
  RenderCustomPlot(simData, input, output)
  
  # Render time series visualization
  RenderTimeSeriesDataTable(timeSeriesDirectory, input, output)
  RenderTimeSeriesPlot(timeSeriesDirectory, input, output)
  
  # Model export
  ModelToXmlHandler(simData, input, output)
  ScenarioToXmlHandler(simData, input, output)
  
  # Execute simulation
  observeEvent(input$execSim, {
    session$sendCustomMessage("runningSimulation", T)
    
    errorLog <- list()
    # Calling handlers to continue the simulation when it receives a warning
    withCallingHandlers({
      # Try Catch to stop the simulation when it receives an error
      tryCatch({
        # Update simulation progress
        simulationProgress <- 0
        initialTime <- as.numeric(input$initialTime)
        finalTime <- as.numeric(input$finalTime)
        
        progressFunction <- function(t) {
          progress <- round((t - initialTime) / finalTime * 100)
          
          if(progress > simulationProgress && progress %% 5 == 0) {
            simulationProgress <<- progress
            session$sendCustomMessage("updateSimulationProgress", toString(progress))
          }
        }

        # Execute simulation
        model <- AssembleModel(simData, input, timeSeriesDirectory, progressFunction)
        alternateScenario <- AssembleAlternateScenario(simData, timeSeriesDirectory)
        
        out <- sdsim::sdSimulate(model = model,
                                 scenario = alternateScenario,
                                 method = input$method,
                                 from = as.numeric(input$initialTime),
                                 to = as.numeric(input$finalTime),
                                 by = as.numeric(input$step))
        
        simData$models[[simData$currentModelId]]$out <- out
        
        # Update custom plot configuration inputs        
        UpdatePlotInput(simData, session)
      },
      # Stop the simulation and show notification when error occurs
      error = function(e) {
        errorOutput <- paste(capture.output(e), collapse = " ")
        errorLog <<- c(errorLog, errorOutput)
      })
    },
    # Show notification when receive warning and continue the simulation
    warning = function(w) {
      warningOutput <- paste(capture.output(w), collapse = " ")
      errorLog <<- c(errorLog, warningOutput)
    })
    
    if(length(errorLog) > 0) {
      showNotification("An error ocurred, please check the log
                       for details", duration = 7)
      output$errorTitle <- renderText("Simulation Log")
      output$errorLog <- renderText(paste(errorLog, collapse = "\n"))
    } else {
      output$errorTitle <- renderText("")
      output$errorLog <- renderText("")
    }
    session$sendCustomMessage("runningSimulation", F)
  })
})

# Send custom message to a script depending on which tab is selected
SendCustomModelBoxMessage <- function(modelBox, session) {
  if(modelBox == "Differential Equations")
    session$sendCustomMessage("shinyAceForceRefresh", "DifferentialEquations")
  if(modelBox == "Parameter Initialization")
    session$sendCustomMessage("shinyAceForceRefresh", "initVars")
  if(modelBox == "Events")
    session$sendCustomMessage("shinyAceForceRefresh", "event")
  if(modelBox == "Trigger")
    session$sendCustomMessage("shinyAceForceRefresh", "root")
  if(modelBox == "Global Functions")
    session$sendCustomMessage("shinyAceForceRefresh", "globalFunctions")
}

# Update method select input based on whether a root function is available
ObserveRootMethod <- function(input, session) {
  observeEvent(input$root, {
    # If root function script is not empty
    if(!grepl(EMPTY_PERL_REGEX, input$root, perl = T)) { 
      # Get method from loaded simulation
      selectedMethod <- input$method
      
      # Check if method is root capable
      if(!grepl("^lsoda$|^lsodar$|^lsode$|^lsodes$|^radau$", selectedMethod)) {
        selectedMethod <- "lsoda"
      }
      
      updateSelectInput(session, "method", 
                        choices = c("lsoda", "lsodar", 
                                    "lsode", "lsodes", 
                                    "radau"),
                        selected = selectedMethod)
    } else { # If root function script is empty
      # Get method from loaded simulation
      selectedMethod <- input$method
      
      updateSelectInput(session, "method", 
                        choices = c("lsoda", "lsode", "lsodes", "lsodar", 
                                    "vode", "daspk", "euler", "rk4", "ode23", 
                                    "ode45", "radau", "bdf", "bdf_d", "adams", 
                                    "impAdams", "impAdams_d"),
                        selected = selectedMethod)
    }
  })
}

# Force refresh ace editor script areas if a file has been uploaded (ace editor bug)
ObserveScriptChanges <- function(input, session) {
  # Sends a custom message to refresh ace editor script when it comes into view
  observeEvent(input$modelBox, {
    SendCustomModelBoxMessage(input$modelBox, session)
  })
  
  # Sends a custom message to refresh the description field when it comes into view
  observeEvent(input$sidebar, {
    SendCustomModelBoxMessage(input$modelBox, session)
    # Force update description script area
    if(input$sidebar == "descriptionPage") {
      session$sendCustomMessage("shinyAceForceRefresh", "description")
    }
  })
}

# Check if any changes were made to variables since the last model upload
ObserveRhandsonChanges <- function(simData, input) {
  observeEvent(input$state, {
    simData$changed$state <- T
  })
  
  observeEvent(input$constant, {
    simData$changed$constant <- T
  })
  
  observeEvent(input$parameter, {
    simData$changed$parameter <- T
  })
  
  observeEvent(input$input, {
    simData$changed$input <- T
  })
  
  observeEvent(input$switch, {
    simData$changed$switch <- T
  })
  
  observeEvent(input$aux, {
    simData$changed$aux <- T
  })
  
  observeEvent(input$staticAux, {
    simData$changed$staticAux <- T
  })
  
  observeEvent(input$connections, {
    simData$changed$connections <- T
  })
  
  observeEvent(input$componentIds, {
    simData$changed$componentIds <- T
  })
}

ShowModelDownloadDialog <- function(simData) {
  showModal(modalDialog(
    title = "Download Model",
    textInput("chooseModelName", "Choose a file name:", value = simData$currentModelId),
    downloadButton("exportModel", "Download",
                   style = paste("color: #000;",
                                 "background-color: #fff;",
                                 "border-color: #000;")),
    h5("* This file can be used to reload the model into the aplication"),
    easyClose = TRUE,
    size = "s"
  ))
}

ShowScenarioDownloadDialog <- function(simData) {
  currentModel <- simData$models[[simData$currentModelId]]
  currentScenarioId <- currentModel$currentScenarioId
  
  showModal(modalDialog(
    title = "Download Scenario",
    textInput("chooseScenarioName", "Choose a file name:", value = currentScenarioId),
    radioButtons("scenarioFileType", label = "Choose a scenario file format:",
                 choices = c("XML", "xlsx (excel)"), selected = "XML", 
                 inline = T),
    downloadButton("exportScenario", "Download",
                   style = paste("color: #000;",
                                 "background-color: #fff;",
                                 "border-color: #000;")),
    h5("* This file can be used to reload the model into the aplication"),
    easyClose = TRUE,
    size = "s"
  ))
}

# Update custom plot configuration inputs
UpdatePlotInput <- function(simData, session) {
  out <- simData$models[[simData$currentModelId]]$out
  
  outputColumnNames <- c(names(out$outTrajectory), 
                         names(out$auxTrajectory), 
                         names(out$timeSeriesTrajectory))
  
  selectedOutputColumnNames <- NULL
  selectXAxisPlot <- "time"
  plotTitle <- ""
  plotXLabel <- ""
  plotYLabel <- ""
  plotType <- "line"
  multipleAxisToggle <- T
  
  plotConfig <- simData$models[[simData$currentModelId]]$plotConfig
  if(!is.null(plotConfig) && 
     plotConfig$selVarPlot %in% outputColumnNames &&
     plotConfig$selectXAxisPlot %in% outputColumnNames) {
    selectedOutputColumnNames <- plotConfig$selVarPlot
    selectXAxisPlot <- plotConfig$selectXAxisPlot
    plotTitle <- plotConfig$plotTitle
    plotXLabel <- plotConfig$plotXLabel
    plotYLabel <- plotConfig$plotYLabel
    plotType <- plotConfig$plotType
    multipleAxisToggle <- plotConfig$multipleAxisToggle
  }
  
  # Update plots select inputs
  outputColumnNames <- outputColumnNames[!outputColumnNames %in% "time"]
  
  updateSelectInput(session, "selVarPlot", choices = outputColumnNames, selected = selectedOutputColumnNames)
  updateSelectInput(session, "selectXAxisPlot", choices = c("time", outputColumnNames), selected = selectXAxisPlot)
  updateTextInput(session, "plotTitle", value = plotTitle)
  updateTextInput(session, "plotXLabel", value = plotXLabel)
  updateTextInput(session, "plotYLabel", value = plotYLabel)
  updateRadioButtons(session, "plotType", selected = plotType)
  updateCheckboxInput(session , "multipleAxisToggle", value = multipleAxisToggle)
}