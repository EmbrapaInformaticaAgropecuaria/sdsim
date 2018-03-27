RenderDataTables <- function(simData, output) {
  # Render output datatable
  output$outTrajectory <- renderDataTable({
    out <- simData$models[[simData$currentModelId]]$out
    if(!is.null(out)) {
      return(out$outTrajectory)
    } else {
      NULL
    }
  }, options = list(
    lengthMenu = c(5, 10, 25, 100, 250),
    pageLength = 10,
    scrollX = TRUE
  ))
  
  # Render auxiliaries datatable
  output$auxTrajectory <- renderDataTable({
    out <- simData$models[[simData$currentModelId]]$out
    if(!is.null(out)) {
      out$auxTrajectory
    } else {
      NULL
    }
  }, options = list(
    lengthMenu = c(5, 10, 25, 100, 250),
    pageLength = 10,
    scrollX = TRUE
  ))
  
  # Render time series datatable
  output$inpTrajectory <- renderDataTable({
    out <- simData$models[[simData$currentModelId]]$out
    if(!is.null(out)) {
      out$timeSeriesTrajectory
    } else {
      NULL
    }
  }, options = list(
    lengthMenu = c(5, 10, 25, 100, 250),
    pageLength = 10,
    scrollX = TRUE
  ))
}

RenderDownloadButtons <- function(simData, output) {
  # Render output download button
  output$exportOutputTrajBt <- renderUI({
    out <- simData$models[[simData$currentModelId]]$out
    if(!is.null(out) && !is.null(out$outTrajectory)) {
      downloadButton("exportOutputTraj", "Export CSV")
    } else {
      NULL
    }
  })
  
  # Render auxiliaries download button
  output$exportAuxTrajBt <- renderUI({
    out <- simData$models[[simData$currentModelId]]$out
    if(!is.null(out) && !is.null(out$auxTrajectory)) {
      downloadButton("exportAuxTraj", "Export CSV")
    } else {
      NULL
    }
  })
  
  # Render auxiliaries download button
  output$exportTimeSeriesTrajBt <- renderUI({
    out <- simData$models[[simData$currentModelId]]$out
    if(!is.null(out) && !is.null(out$timeSeriesTrajectory)) {
      downloadButton("exportTimeSeriesTraj", "Export CSV")
    } else {
      NULL
    }
  })
}

# Create download button for exporting the simulation result table
TrajectoriesDownloadHandler <- function(simData, output) {
  output$exportOutputTraj <- downloadHandler(
    filename = function() {
      currentModel <- simData$models[[simData$currentModelId]]
      currentScenario <- currentModel$scenarios[[currentModel$currentScenarioId]]
      
      paste0(currentModel$modelId, "(", currentScenario$id, ")_state_trajectory.csv")
    },
    content = function(file) {
      out <- simData$models[[simData$currentModelId]]$out
      write.table(data.frame(out$outTrajectory), file, sep = ";", 
                  dec = ".", row.names = FALSE)
    }
  )
  
  output$exportAuxTraj <- downloadHandler(
    filename = function() {
      currentModel <- simData$models[[simData$currentModelId]]
      currentScenario <- currentModel$scenarios[[currentModel$currentScenarioId]]
      
      paste0(currentModel$modelId, "(", currentScenario$id, ")_auxiliary_trajectory.csv")
    },
    content = function(file) {
      out <- simData$models[[simData$currentModelId]]$out
      write.table(data.frame(out$auxTrajectory), file, sep = ";", 
                  dec = ".", row.names = FALSE)
    }
  )
  
  output$exportTimeSeriesTraj <- downloadHandler(
    filename = function() {
      currentModel <- simData$models[[simData$currentModelId]]
      currentScenario <- currentModel$scenarios[[currentModel$currentScenarioId]]
      
      paste0(currentModel$modelId, "(", currentScenario$id, ")_time_series_trajectory.csv")
    },
    content = function(file) {
      out <- simData$models[[simData$currentModelId]]$out
      write.table(data.frame(out$timeSeriesTrajectory), file, sep = ";", 
                  dec = ".", row.names = FALSE)
    }
  )
}

RenderCustomPlot <- function(simData, input, output) {
  # Render custom plot
  output$customPlot <- renderPlot({
      tryCatch({
        out <- simData$models[[simData$currentModelId]]$out
        SavePlotConfig(simData, input)
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
          out$plot(variables, xlab = xlab, ylab = ylab, 
                   multipleYAxis = input$multipleAxisToggle, 
                   main = main, type = type, units = input$showUnitToggle)
        }
      },
      error = function(e) {},
      warning = function(w) {})
  })
}

# Save current model's plot configuration for custom plot
SavePlotConfig <- function(simData, input) {
  plotConfig <- list(
    selVarPlot = input$selVarPlot,
    selectXAxisPlot = input$selectXAxisPlot,
    plotTitle = input$plotTitle,
    plotXLabel = input$plotXLabel,
    plotYLabel = input$plotYLabel,
    plotType = input$plotType,
    multipleAxisToggle = input$multipleAxisToggle
  )
  simData$models[[simData$currentModelId]]$plotConfig <- plotConfig
}

RenderTimeSeriesDataTable <- function(timeSeriesDirectory, input, output) {
  # Render time series data table
  output$timeSeriesTable <- renderDataTable({
    withCallingHandlers({
      tryCatch({
        input$viewTimeSeries
        file <- paste0(timeSeriesDirectory, "/", input$selectTs)
        read.table(file = file, header = T, sep = ",")
      },
      error = function(e) {
        errorOutput <- paste(capture.output(e), collapse = " ")
        showNotification(errorOutput, duration = 7)
      })
    },
    warning = function(w) {
      warningOutput <- paste(capture.output(w), collapse = " ")
      showNotification(warningOutput, duration = 7)
    })
  }, options = list(
    lengthMenu = c(5, 10, 25),
    pageLength = 5,
    scrollX = TRUE
  ))
}

RenderTimeSeriesPlot <- function(timeSeriesDirectory, input, output) {
  # Render time series plot
  output$timeSeriesPlot <- renderPlot({
    withCallingHandlers({
      tryCatch({
        input$viewTimeSeries
        file <- paste0(timeSeriesDirectory, "/", input$selectTs)
        plotType <- switch(input$TsPlotType, "line" = "l", "point" = "p")
        plot(read.table(file = file, header = T, sep = ","), 
             type = plotType, lwd = 1.5)
      },
      error = function(e) {
        errorOutput <- paste(capture.output(e), collapse = " ")
        showNotification(errorOutput, duration = 7)
      })
    },
    warning = function(w) {
      warningOutput <- paste(capture.output(w), collapse = " ")
      showNotification(warningOutput, duration = 7)
    })
  })
}
