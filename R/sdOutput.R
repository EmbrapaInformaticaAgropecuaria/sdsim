#' Class Representation of a Simulation Output
#' 
#' Represents a model simulation output obtained with the 
#' \code{\link{sdSimulate}} function. 
#' It contains the output trajectory, the auxiliary equations 
#' trajectory, the input time series variables trajectory, the simulation 
#' diagnostics and the postProcess function return value.
#' 
#' It has methods for saving the output, graphically visualizing the stored 
#' trajectories 
#' (\code{$plot()}) and for summarizing the trajectories (\code{$summary()}). 
#' 
#' @field outputId The output id, generated in the initialization as: 
#' "Simulation Output " + Sys.time(). 
#' @field model The model simulated. A \code{\link{sdStaticModelClass}}, a 
#' \code{\link{sdModelClass}} or a \code{\link{sdCoupledModelClass}} object.
#' @field scenario The \code{\link{sdScenarioClass}} object used in the 
#' simulation, if any.
#' @field outTrajectory In case of a \code{\link{sdStaticModelClass}}: 
#' a data.frame 
#' with the algebraic equations trajectories of the simulation. This data.frame 
#' have up to as many rows as elements in the time sequence and as many columns 
#' as variables in the \code{eq} list plus an additional column (the first) for 
#' the time sequence values. The names of the \code{eq} list will be 
#' used to label the columns of the algebraic equations data.frame.
#' 
#' In case of a \code{\link{sdModelClass}} or a 
#' \code{\link{sdCoupledModelClass}}: a 
#' data.frame with the ODE output of the simulation. This data.frame have up to 
#' as many rows as elements in the time sequence and as many columns as 
#' variables in the \code{state} list plus the number of auxiliary values 
#' returned in the second and following elements of the return list from 
#' \code{DifferentialEquations}, plus an additional column (the first) for the 
#' time sequence values. There will be one row for each element in times unless 
#' the integrator returns with an unrecoverable error. 
#' If the \code{state} list and the auxiliary values have a names attribute, 
#' they will be used to label the columns of the output data.frame. 
#' @field auxTrajectory Just in case of a \code{\link{sdModelClass}} or a 
#' \code{\link{sdCoupledModelClass}}:
#' A data.frame with the auxiliary equations trajectories 
#' of the simulation. This data.frame have up to as many rows as elements in the 
#' time sequence and as many columns as variables in the \code{aux} list plus an 
#' additional column (the first) for the time sequence values. The 
#' names of the \code{aux} list will be used to label the columns of the 
#' auxiliary equations data.frame.
#' @field timeSeriesTrajectory A data.frame with the time series variables 
#' simulation trajectories. This data.frame have up to as many rows as elements 
#' in the time sequence and as many columns as variables in the 
#' \code{interpolation_} list plus an additional column (the first) for the 
#' time sequence values. The names of the \code{input} time series variables 
#' will be used to label the columns of the time series data.frame.
#' @field diagnostics A string with the simulation diagnostics, e.g. 
#' number of steps taken, the last step size, root informations, etc. See
#' \code{\link[deSolve]{diagnostics}} for more informations.
#' @field postProcess The return value of the \code{model} 
#' \code{PostProcessVars} function.
#'
#' @section Public Methods Definition:  
#' \describe{
#' \item{\code{$print()}}{Print the last 10 rows of the data.frames 
#' trajectories.}
#' 
#' \item{\code{$summary()}}{Print the data.frame trajectories summary.}
#' 
#' \item{\code{$plot(..., xlab = NULL, ylab = NULL, main = NULL, sub = NULL, 
#' type = "l", maxRow = 2, maxCol = 2, plotSymbol = 1, symbolSize = 2.5,
#' legendPosition = "topright", multipleYAxis = F, units = T, 
#' col = c("black", "red", "blue", "green4", "darkorange", "darkmagenta", 
#' "khaki4", "cyan", "gold2", "hotpink"))}}{Plot the formulas given in the ... 
#' argument using the data.frames trajectories.
#' 
#' \strong{Arguments}
#' 
#' \describe{
#' \item{...}{A list of strings with each element describing a formula with the 
#' variables to plot. 
#' 
#' The formulas must follow the standard: \code{"y"}, \code{"y1 y2 yn"}, 
#' \code{"y ~ x"} or \code{"y1 y2 yn ~ x"}.
#' 
#' Where \code{yn} are the n variables names to be plotted in the y-axis, 
#' separated by a white space, or a single \code{y} variable name; 
#' 
#' the symbol \code{~} is the separator to be used in the formulas when passing 
#' the x variable; 
#' 
#' and \code{x} is the name of the variable to be plotted in the x-axis, by 
#' default the simulation time sequence will be used in the x-axis and no 
#' variable name need to be informed (e.g. omitting "~ x"). 
#' 
#' When plotting more than one variable in the y-axis consider using the argument 
#' \code{multipleYAxis = TRUE}.}
#' \item{xlab}{A list with the labels for the plots x-axis. Must have the same 
#' length as the argument '...'.}
#' \item{ylab}{A list with the labels for the plots y-axis. Must have the same 
#' length as the argument '...'. In case of plotting multiple y variables and 
#' setting \code{multipleYAxis = TRUE} the corresponding elements of this list 
#' must be a vector with the same length of y variables to be plotted (n).}
#' \item{main}{A list with the overall titles for the plots. Must have the same 
#' length as the argument '...'.}
#' \item{sub}{A list with the sub titles for the plots. Must have the same 
#' length as the argument '...'.}
#' \item{type}{What type of plot should be drawn. See \code{\link{plot}} for the
#' available options. Default is 'l', for lines.}
#' \item{maxRow}{The number of rows in the plots layout. Default is 2.}
#' \item{maxCol}{The number of columns in the plots layout. Default is 2.}
#' \item{plotSymbol}{The line type (lty) or the character to be used in plotting 
#' points (pch). See \code{\link[graphics]{par}}, the parameters 
#' 'lty' and 'pch', for the detailed information about the possible values and 
#' their interpretation. Default is 1.}
#' \item{symbolSize}{The line width, a positive number, defaulting to 2.5. The 
#' interpretation is device-specific, and some devices do not implement line 
#' widths less than one. (See the help on the device for details of the 
#' interpretation and the help on \code{\link[graphics]{par}} for other 
#' details in the parameter 'lwd'.)}
#' \item{legendPosition}{A single keyword from the list "bottomright", "bottom", 
#' "bottomleft", "left", "topleft", "top", "topright", "right" and "center". 
#' This places the legend on the inside of the plot frame at the given location. 
#' Partial argument matching is used. Default is "topright".}
#' \item{multipleYAxis}{Logical; if TRUE, plot one y-axis range per y variable. 
#' If FALSE plot all the y variables in the same y-axis range. Default is 
#' FALSE.}
#' \item{units}{Logical; If TRUE, concatenate the unit value stored in the model
#' default scenario to the x-axis and y-axis label, when there is one variable 
#' per axis or \code{multipleYAxis = TRUE}. Default is FALSE.}
#' \item{col}{A vector with the color of points or lines appearing in the 
#' legend. A selection of colors is used as default. If the amount of variables 
#' to be plotted is greater than the length of the selected colors, extra colors 
#' from the \code{\link[grDevices]{colors}} function will be used.}
#' }}
#' 
#' \item{\code{$saveSimulationTrajectories(path = "directory")}}{Save the 
#' simulation trajectories to text files, and the model and the scenario to XML 
#' files inside the \code{path} directory.
#' 
#' \strong{Arguments}
#' 
#' \describe{
#' \item{path}{A string with the directory name to save the files. If missing 
#' uses the \code{outputId} to name the created directory in the current working
#' directory.}
#' }}
#' } 
#' @name sdOutput
#' @examples 
#' ## Let's load the Arenstorf model from the sdsim repository and then simulate
#' # it to explore the output object functionalities 
#' arenstorf <- sdLoadModel(file = "Arenstorf", repository = TRUE)
#' outaren <- sdSimulate(model = arenstorf)
#' 
#' # print the output object to visualize the simulation diagnostics and 
#' # the last 10 lines of the variables trajectories
#' outaren$print()
#' 
#' # print the summary of all the variables trajectories
#' outaren$summary()
#' 
#' # visualize the entire output trajectory until the max.print
#' outaren$outTrajectory
#' 
#' # visualize the circular rotation of the bodies and
#' # the coordinates derivatives over time,
#' # concatenating the units of the variables stored in the default scenario;
#' outaren$plot("y1 ~ y2", "dy1 dy2", units = TRUE,
#'              main = c("Rotation Coordinates", 
#'                       "Rotation Coordinates Derivatives"))
#' 
#' # use multipleYAxis = TRUE for a better visualization of the derivatives
#' outaren$plot("dy1 dy2", multipleYAxis = TRUE,
#'              main = "Rotation Coordinates Derivatives")
#' 
#' # save the simulation trajectories for further use
#' outaren$saveSimulationTrajectories("Arenstorf")
sdOutputClass <- R6::R6Class(
  classname = "sdOutputClass",
  public = list(
    initialize = function(outTrajectory, auxTrajectory, 
                          timeSeriesTrajectory, model, scenario, diagnostics, 
                          postProcess)
    {
      if (!missing(outTrajectory))
        private[["poutTrajectory"]] <- outTrajectory
      
      if (!missing(auxTrajectory))
        private[["pauxTrajectory"]] <- auxTrajectory
      
      if (!missing(timeSeriesTrajectory))
        private[["ptimeSeriesTrajectory"]] <- timeSeriesTrajectory
      
      if (!missing(model))
        private[["pmodel"]] <- model
      
      if (!missing(scenario))
        private[["pscenario"]] <- scenario
      
      if (!missing(diagnostics))
        private[["pdiag"]] <- diagnostics
      
      if (!missing(postProcess))
        private[["ppostProcess"]] <- postProcess
      
      private$poutputId <- paste0("Simulation Output ", Sys.time())
    },
    print = function()
    {
      cat("\n<SDOutput ")
      if (inherits(private$pmodel, "sdModelClass"))
        cat("sdModel ID = ", private$pmodel$modelId, sep = "")
      else if (inherits(private$pmodel, "sdCoupledModelClass"))
        cat("sdCoupledModel ID = ", private$pmodel$coupledModelId, sep = "")
      else if (inherits(private$pmodel, "sdStaticModelClass"))
        cat("sdStaticModel ID = ", private$pmodel$staticModelId, sep = "")
      
      if (!is.null(private$pscenario))
        cat(" sdScenario ID = ", private$pscenario$scenarioId, ">\n\n", 
            sep = "")
      else 
        cat(">\n\n")
      
      # static models do not have diagnostics
      if (!inherits(private$pmodel, "sdStaticModelClass"))
      {
        cat(indent("$Simulation Diagnostics", indent = 4))
        cat(indent(private$pdiag, indent = 4))
        cat("\n\n")
      }
      
      if (!is.null(private[["poutTrajectory"]]))
      {
        cat(indent("$Output Trajectories", indent = 4), sep = "\n")
        cat(indent(paste(capture.output(tail(private[["poutTrajectory"]], n = 10, 
                                             addrownums = FALSE)), 
                         collapse =  "\n"), indent = 4))
        cat("\n\n")
      }
      
      if (!is.null(private[["pauxTrajectory"]]))
      {
        # static models do not have auxiliaries
        if (!inherits(private$pmodel, "sdStaticModelClass"))
        {
          cat(indent("$Auxiliary Trajectories", indent = 4), sep = "\n")
          cat(indent(paste(capture.output(tail(private[["pauxTrajectory"]], n = 10, 
                                               addrownums = FALSE)), 
                           collapse =  "\n"), indent = 4))
          cat("\n\n")
        }
      }
      
      if (!is.null(private[["ptimeSeriesTrajectory"]]))
      {
        cat(indent("$Time Series Trajectories", indent = 4), sep = "\n")
        cat(indent(paste(capture.output(tail(private[["ptimeSeriesTrajectory"]], 
                                             n = 10, 
                                             addrownums = FALSE)), 
                         collapse =  "\n"), indent = 4))
        cat("\n")
      }
    },
    summary = function()
    {
      if (!is.null(private[["poutTrajectory"]]))
      {
        sdOutputMsg$summary1()
        print(summary(private[["poutTrajectory"]]))
      }
      
      if (!is.null(private[["pauxTrajectory"]]))
      {
        sdOutputMsg$summary2()
        print(summary(private[["pauxTrajectory"]]))
      }
      
      if (!is.null(private[["ptimeSeriesTrajectory"]]))
      {
        sdOutputMsg$summary3()
        print(summary(private[["ptimeSeriesTrajectory"]]))
      }
    },
    plot = function(..., xlab = NULL, ylab = NULL, main = NULL, 
                    sub = NULL, type = "l", maxRow = 2, maxCol = 2, 
                    plotSymbol = 1, symbolSize = 2.5,
                    legendPosition = "topright", multipleYAxis = F,
                    units = F,
                    col = c("black", "red", "blue", "green4", "darkorange", 
                            "darkmagenta", "khaki4", "cyan", "gold2", "hotpink")
    )
    {
      if (is.null(legendPosition))
        legendPosition <- NA
      
      which <- list(...)
      
      data <- private[["poutTrajectory"]]
      
      # retrieve the model default scenario
      if (inherits(private$pmodel, "sdModelClass") || 
          inherits(private$pmodel, "sdStaticModelClass"))
        dfscen <- private$pmodel$defaultScenario
      else if (inherits(private$pmodel, "sdCoupledModelClass"))
        dfscen <- private$pmodel$defaultCoupledScenario
      
      # all the labels must be provided, or any will be used
      if (!is.null(xlab) && length(which) != length(xlab))
      {
        sdOutputMsg$plot1(private$poutputId, "xlab")
        xlab <- NULL
      }
      if (!is.null(ylab) && length(which) != length(ylab))
      {
        sdOutputMsg$plot1(private$poutputId, "ylab")
        ylab <- NULL
      }
      if (!is.null(main) && length(which) != length(main))
      {
        sdOutputMsg$plot1(private$poutputId, "main")
        main <- NULL
      }
      if (!is.null(sub) && length(which) != length(sub))
      {
        sdOutputMsg$plot1(private$poutputId, "sub")
        sub <- NULL
      }
      
      if (length(which) == 0)
        which <- colnames(data)[2:ncol(data)]
      else
      {
        if (!is.character(unlist(which, recursive = T)))
        {
          which <- "all"
          sdOutputMsg$plot2(poutputId)
        }
        else
        {
          if (!is.null(private[["pauxTrajectory"]]))
            data <- merge(data, private[["pauxTrajectory"]],
                          sort = FALSE, by.x = "time", by.y = "time")
          
          if (!is.null(private[["ptimeSeriesTrajectory"]]))
            data <- merge(data, private[["ptimeSeriesTrajectory"]],
                          sort = FALSE, by.x = "time", by.y = "time")
        }
      }
      
      if (!is.null(which) && length(which) == 1 && which == "all")
        which <- colnames(data)[2:ncol(data)]
      
      ncol <- min(ceiling(sqrt(length(which))), maxCol)
      nrow <- min(ceiling(length(which)/ncol), maxRow)
      
      mar <- par()$mar
      mfrow <- par()$mfrow
      par(mfrow=c(nrow, ncol))
      
      # plot each formula from ... in a separeted plot
      plots <- list() # list of plots size = i
      i <- 1 # number of plots
      for (column in which)
      {
        xaxis <- "time"
        yaxis <- column
        xlabel <- ""
        ylabel <- ""
        mainlabel <- ""
        sublabel <- ""
        
        if (grepl("~", column))
        {
          vars <- unlist(strsplit(column, "~"))
          yaxis <- vars[[1]]
          xaxis <- vars[[2]]
        }
        
        # set x label
        if (!is.null(xlab))
          xlabel <- xlab[[i]]
        else
          xlabel <- xaxis
        
        # set main title
        if (!is.null(main))
        {
          ylabel <- yaxis
          mainlabel <- main[[i]]
        }
        else
          mainlabel <- yaxis
        
        # set y label
        if (!is.null(ylab))
          ylabel <- ylab[[i]]
        
        # set sub title
        if (!is.null(sub))
          sublabel <- sub[[i]]
        
        yaxis <- gsub("^\\s+|\\s+$", "", yaxis)
        xaxis <- gsub("^\\s+|\\s+$", "", xaxis)
        
        # Separate y axis variables
        yaxisArray <- unlist(strsplit(yaxis, " "))
        
        # check if all the variables names are valid columns names
        if (!all((c(xaxis, yaxisArray) %in% names(data)))) 
        {
          sdOutputMsg$plot3(private$poutputId, xaxis, yaxisArray, names(data))
          
          if (!(xaxis %in% names(data)))
          {
            sdOutputMsg$plot4(private$poutputId, xaxis)
            xaxis <- "time"
          }
          
          # remove the not valid columns
          yaxisArray <- yaxisArray[
            yaxisArray %in% yaxisArray[(yaxisArray %in% names(data))]]
          
          # skip if no valid column are left
          if (length(yaxisArray) == 0)
            next()
        }
        nYAxis <- length(yaxisArray)
        
        if (multipleYAxis)
          par(mar=c(5, 1 + 3.5*nYAxis, 4, 4) + 0.1)
        
        # add the x-axis unit if any
        if (units && !is.null(dfscen$unit[[xaxis]]))
          xlabel <- paste0(xlabel, " (", dfscen$unit[[xaxis]], ")")
        
        # Create empty plot within range of xaxis and yaxis
        plot(x = NULL,
             y = NULL,
             axes = !(multipleYAxis && nYAxis > 1),
             main = mainlabel,
             sub = sublabel,
             ylab = "",
             xlab = xlabel,
             xlim = range(data[,xaxis]),
             ylim = range(data[,yaxisArray]))
        
        # get more colors
        if (nYAxis > length(col))
          col <- unique(c(col, colors(distinct = T)))
        
        # plot each y variable
        for (j in 1:nYAxis)
        {
          if (multipleYAxis && nYAxis > 1) # when plotting multiple y-axis
          {
            par(new=T)
            plot(x = data[,xaxis],
                 y = data[,yaxisArray[j]],
                 axes = F,
                 main = "",
                 ylab = "",
                 xlab = "",
                 xlim = range(data[,xaxis]),
                 ylim = range(data[,yaxisArray[j]]),
                 col = col[j],
                 lty = plotSymbol,
                 pch = plotSymbol,
                 lwd = symbolSize,
                 type = type)
            
            # deslocate the y axis
            axis(2, ylim=range(data[, yaxisArray[j]]), col="black", lwd = 1.5,
                 line = 3.5*j - 3.5)
            
            # insert the ylabel if compatible size
            if (length(ylabel) == nYAxis)
              labeltext <- paste(j, ".", paste(ylabel[[j]], collapse = " "))
            else
              labeltext <- paste(j, ".", yaxisArray[[j]])
            
            # add y-axis unit
            if (units && !is.null(dfscen$unit[[yaxisArray[[j]]]]))
              labeltext <- paste0(labeltext, " (", 
                                  dfscen$unit[[yaxisArray[[j]]]], ")")
            
            # add the ylabel
            mtext(2, text = labeltext, line = 3.5*j - 1.5)
          }
          else 
          { # multipleYAxis = FALSE
            lines(data[,xaxis],
                  y = data[,yaxisArray[j]],
                  col = col[j],
                  lty = plotSymbol,
                  pch = plotSymbol,
                  lwd = symbolSize,
                  type = type)
            
            # add the y label
            if (nYAxis == 1)
            {
              if (units && !is.null(dfscen$unit[[yaxisArray[[j]]]]))
                ylabel <- paste0(ylabel, " (", 
                                 dfscen$unit[[yaxisArray[[j]]]], ")")
              
              mtext(2, text = paste(ylabel, collapse = " "), line = 2.5)
            }
            else
              mtext(2, text = paste(ylabel, collapse = " "), line = 2.5)
          }
        }
        
        if (nYAxis > 1)
        {
          if (multipleYAxis)
            legend(legendPosition, paste(1:nYAxis, '.', yaxisArray), 
                   lty = 1, lwd = symbolSize, col = col)
          else
            legend(legendPosition, yaxisArray, 
                   lty = 1, lwd = symbolSize, col = col)
        }
        
        if (multipleYAxis)
          axis(1, pretty(range(data[,xaxis])), xlim = range(data[,xaxis]), 
               lwd=1.5, line = 0)
        
        # store the printed plots
        plots[[i]] <- recordPlot()
        i <- i + 1
      }
      
      # restore the parameters
      par(mfrow = mfrow, mar = mar)
      
      invisible(plots)
    },
    saveSimulationTrajectories = function(path = "directory")
    {
      if (missing(path))
        path <- private$poutputId
      
      if (!dir.exists(path)) 
        dir.create(path = paste0(path, "/"), recursive = TRUE)
      
      # save the trajectories
      if (!is.null(private$poutTrajectory))
        write.csv(x = private$poutTrajectory, 
                  file = paste0(path, "/outputTrajectory.csv"),
                  row.names = F)
      
      if (!is.null(private$pauxTrajectory) &&
          !inherits(private$pmodel, "sdStaticModelClass"))
        write.csv(x = private$pauxTrajectory, 
                  file = paste0(path, "/auxTrajectory.csv"), row.names = F)
      
      if (!is.null(private$ptimeSeriesTrajectory))
        write.csv(x = private$ptimeSeriesTrajectory, 
                  file = paste0(path, "/timeSeriesTrajectory.csv"),
                  row.names = F)
      
      # save model and scenario
      if (inherits(private$pmodel, "sdModelClass"))
        private$pmodel$saveToXml(paste0(path, "/", private$pmodel$modelId, 
                                        ".xml"))
      else if (inherits(private$pmodel, "sdCoupledModelClass"))
        private$pmodel$saveToXml(paste0(path, "/", 
                                        private$pmodel$coupledModelId, 
                                        ".xml"))
      else if (inherits(private$pmodel, "sdStaticModelClass"))
        private$pmodel$saveToXml(paste0(path, "/", 
                                        private$pmodel$staticModelId, 
                                        ".xml"))
      
      if (!is.null(private$pscenario))
        private$pscenario$saveToXml(paste0(path, "/", 
                                           private$pscenario$scenarioId, 
                                           ".xml"))
    }
  ),
  active = list(
    model = function()
    {
      return(private[["pmodel"]])
    },
    scenario = function()
    {
      return(private[["pscenario"]])
    },
    outTrajectory = function()
    {
      if (length(private[["poutTrajectory"]]) == 0)
        return(NULL)
      return(private[["poutTrajectory"]])
    },
    auxTrajectory = function()
    {
      if (length(private[["pauxTrajectory"]]) == 0)
        return(NULL)
      return(private[["pauxTrajectory"]])
    },
    timeSeriesTrajectory = function()
    {
      if (length(private[["ptimeSeriesTrajectory"]]) == 0)
        return(NULL)
      return(private[["ptimeSeriesTrajectory"]])
    },
    PostProcess = function()
    {
      return(private[["ppostProcess"]])
    },
    diagnostics = function()
    {
      if (!is.null(private[["pdiag"]]))
      {
        cat(private[["pdiag"]])
        invisible(private[["pdiag"]])
      }
      else
        return(NULL)
    }
  ),
  private = list(
    poutputId = NULL,
    poutTrajectory = NULL,
    pauxTrajectory = NULL,
    ptimeSeriesTrajectory = NULL,
    ppostProcess = NULL,
    pmodel = NULL,
    pscenario = NULL,
    pdiag = NULL
  )
)