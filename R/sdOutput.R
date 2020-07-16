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
#' \code{\link{sdOdeModelClass}} or a \code{\link{sdCoupledModelClass}} object.
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
#' In case of a \code{\link{sdOdeModelClass}} or a 
#' \code{\link{sdCoupledModelClass}}: a 
#' data.frame with the ODE output of the simulation. This data.frame have up to 
#' as many rows as elements in the time sequence and as many columns as 
#' variables in the \code{state} list plus the number of auxiliary values 
#' returned in the second and following elements of the return list from 
#' \code{ode}, plus an additional column (the first) for the 
#' time sequence values. There will be one row for each element in times unless 
#' the integrator returns with an unrecoverable error. 
#' If the \code{state} list and the auxiliary values have a names attribute, 
#' they will be used to label the columns of the output data.frame. 
#' @field auxTrajectory Just in case of a \code{\link{sdOdeModelClass}} or a 
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
#' @field postProcessOut The return value of the \code{model} 
#' \code{postProcess} function.
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
#' \item{\code{$saveSimulationOutput(path = "directory", scenarioXlsx = TRUE)}}{
#' Save the simulation trajectories to text files, the model to XML and the 
#' scenario to Xlsx or XML 
#' files inside the \code{path} directory.
#' 
#' \strong{Arguments}
#' 
#' \describe{
#' \item{path}{A string with the directory name to save the files. If missing 
#' uses the \code{outputId} to name the created directory in the current working
#' directory.}
#' \item{scenarioXlsx}{Logical; If TRUE, save the scenario to a xlsx file; If 
#' FALSE save the scenario to a XML file. Default is TRUE.}
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
#' outaren$saveSimulationOutput("Arenstorf")
sdOutputClass <- R6::R6Class(
  classname = "sdOutput",
  public = list(
    initialize = function(outTrajectory, auxTrajectory, 
                          timeSeriesTrajectory, model, scenario, diagnostics, 
                          postProcessOut) { 
      if (!missing(outTrajectory))
        private[["pOutTrajectory"]] <- outTrajectory
      
      if (!missing(auxTrajectory))
        private[["pAuxTrajectory"]] <- auxTrajectory
      
      if (!missing(timeSeriesTrajectory))
        private[["pTimeSeriesTrajectory"]] <- timeSeriesTrajectory
      
      if (!missing(model))
        private[["pModel"]] <- model
      
      if (!missing(scenario))
        private[["pScenario"]] <- scenario
      
      if (!missing(diagnostics))
        private[["pDiag"]] <- diagnostics
      
      if (!missing(postProcessOut))
        private[["pPostProcessOut"]] <- postProcessOut
      
      private$pOutputId <- paste0("Simulation Output ", Sys.time())
    },
    print = function() { 
      cat("<", class(self)[[1]], ">\n", sep = "")
      cat(indent(paste0("$",class(private$pModel)[[1]]), indent = 4), sep = "\n")
      cat(indent(private$pModel$id, indent = 4), sep = "\n")
      cat("\n")
      
      if (!is.null(private$pScenario)) { 
        cat(indent("$sdScenario", indent = 4), sep = "\n")
        cat(indent(private$pScenario$id, indent = 4), sep = "\n")
        cat("\n")
      }
      # static models do not have diagnostics
      if (!inherits(private$pModel, sdStaticModelClass$classname)) { 
        cat(indent("$Simulation Diagnostics", indent = 4))
        cat(indent(private$pDiag, indent = 4))
        cat("\n\n")
      }
      
      if (!is.null(private[["pOutTrajectory"]])) { 
        cat(indent("$Output Trajectories", indent = 4), sep = "\n")
        if(is.vector(private$pOutTrajectory)) {
          names <- names(private$pScenario$state)
          df <- data.frame(t(matrix(private$pOutTrajectory, nrow = length(names))))
          colnames(df) <- names
          cat(indent(paste(capture.output(tail(df, n = 10, 
                                               addrownums = FALSE)), 
                           collapse =  "\n"), indent = 4))
        } 
        else
          cat(indent(paste(capture.output(tail(private$pOutTrajectory, n = 10, 
                                               addrownums = FALSE)), 
                           collapse =  "\n"), indent = 4))
        cat("\n\n")
      }
      
      if (!is.null(private[["pAuxTrajectory"]])) { 
        # static models do not have auxiliaries
        if (!inherits(private$pModel, sdStaticModelClass$classname)) { 
          cat(indent("$Auxiliary Trajectories", indent = 4), sep = "\n")
          if(is.vector(private$pAuxTrajectory)) {
            names <- names(private$pModel$aux)
            df <- data.frame(t(matrix(private$pAuxTrajectory, nrow = length(names))))
            colnames(df) <- names
            cat(indent(paste(capture.output(tail(df, n = 10, 
                                                 addrownums = FALSE)), 
                             collapse =  "\n"), indent = 4))
          } else 
            cat(indent(paste(capture.output(tail(private$pAuxTrajectory, n = 10, 
                                                 addrownums = FALSE)), 
                             collapse =  "\n"), indent = 4))
          cat("\n\n")
        }
      }
      
      if (!is.null(private[["pTimeSeriesTrajectory"]])) { 
        cat(indent("$Time Series Trajectories", indent = 4), sep = "\n")
        cat(indent(paste(capture.output(tail(private[["pTimeSeriesTrajectory"]], 
                                             n = 10, 
                                             addrownums = FALSE)), 
                         collapse =  "\n"), indent = 4))
        cat("\n")
      }
    },
    summary = function() { 
      if (!is.null(private[["pOutTrajectory"]])) { 
        message(sdOutputMsg$summary1)
        print(summary(private[["pOutTrajectory"]]))
      }
      
      if (!is.null(private[["pAuxTrajectory"]])) { 
        message(sdOutputMsg$summary2)
        print(summary(private[["pAuxTrajectory"]]))
      }
      
      if (!is.null(private[["pTimeSeriesTrajectory"]])) { 
        message(sdOutputMsg$summary3)
        print(summary(private[["pTimeSeriesTrajectory"]]))
      }
    },
    plot = function(..., xlab = NULL, ylab = NULL, main = NULL, 
                    sub = NULL, type = "l", maxRow = 2, maxCol = 2, 
                    plotSymbol = 1, symbolSize = 2.5,
                    legendPosition = "topright", multipleYAxis = F,
                    units = F,
                    col = c("black", "red", "blue", "green4", "darkorange", 
                            "darkmagenta", "khaki4", "cyan", "gold2", "hotpink")
    ) { 
      if (is.null(legendPosition))
        legendPosition <- NA
      
      which <- list(...)
      
      data <- private[["pOutTrajectory"]]
      
      # retrieve the model default scenario
      dfscen <- private$pModel$defaultScenario
      
      # all the labels must be provided, or any will be used
      if (!is.null(xlab) && length(which) != length(xlab)) { 
        warning(sprintf(sdOutputMsg$plot1,private$pOutputId, "xlab","xlab"))
        xlab <- NULL
      }
      if (!is.null(ylab) && length(which) != length(ylab)) { 
        warning(sprintf(sdOutputMsg$plot1,private$pOutputId, "ylab","ylab"))
        ylab <- NULL
      }
      if (!is.null(main) && length(which) != length(main)) { 
        warning(sprintf(sdOutputMsg$plot1,private$pOutputId, "main","main"))
        main <- NULL
      }
      if (!is.null(sub) && length(which) != length(sub)) { 
        warning(sprintf(sdOutputMsg$plot1,private$pOutputId, "sub","sub"))
        sub <- NULL
      }
      
      if (length(which) == 0) {
        which <- colnames(data)[2:ncol(data)]
      } else { 
        if (!is.character(unlist(which, recursive = T))) { 
          which <- "all"
          warning(sprintf(sdOutputMsg$plot2,private$pOutputId))
        } else { 
          if (!is.null(private[["pAuxTrajectory"]]))
            data <- merge(data, private[["pAuxTrajectory"]],
                          sort = FALSE, by.x = "time", by.y = "time")
          
          if (!is.null(private[["pTimeSeriesTrajectory"]]))
            data <- merge(data, private[["pTimeSeriesTrajectory"]],
                          sort = FALSE, by.x = "time", by.y = "time")
        }
      }
      
      if (!is.null(which) && length(which) == 1 && which == "all")
        which <- colnames(data)[2:ncol(data)]
      
      ncol <- min(ceiling(sqrt(length(which))), maxCol)
      nrow <- min(ceiling(length(which)/ncol), maxRow)
      
      # save par settings
      mar <- par()$mar
      mfrow <- par()$mfrow
      par(mfrow=c(nrow, ncol))
      
      try({
        
        # plot each formula from ... in a separeted plot
        plots <- list() # list of plots size = i
        i <- 1 # number of plots
        for (column in which) { 
          xaxis <- "time"
          yaxis <- column
          xlabel <- ""
          ylabel <- ""
          mainlabel <- ""
          sublabel <- ""
          
          if (grepl("~", column)) { 
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
          if (!is.null(main)) { 
            ylabel <- yaxis
            mainlabel <- main[[i]]
          } else {
            mainlabel <- yaxis
          }
          
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
          if (!all(yaxisArray %in% names(data))) { 
            warning(sprintf(sdOutputMsg$plot3, private$pOutputId, 
                            paste(yaxisArray[!(yaxisArray %in% names(data))], 
                                  collapse = ", ")))
            
            # remove the not valid columns
            yaxisArray <- yaxisArray[
              yaxisArray %in% yaxisArray[(yaxisArray %in% names(data))]]
            
            # skip if no valid column are left
            if (length(yaxisArray) == 0)
              next()
          }
          
          if (!(xaxis %in% names(data))) { 
            warning(sprintf(sdOutputMsg$plot4,private$pOutputId, xaxis))
            xaxis <- "time"
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
          
          # check the ylabel size when plotting more than one YAxis
          if (!is.null(ylab) && ylabel != "" && multipleYAxis && 
              length(ylabel) < nYAxis)
            warning(sprintf(sdOutputMsg$plot1,private$pOutputId, "ylab","ylab"))
          
          # plot each y variable
          for (j in 1:nYAxis) { 
            if (multipleYAxis && nYAxis > 1) { # when plotting multiple y-axis
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
            } else {  # multipleYAxis = FALSE
              lines(data[,xaxis],
                    y = data[,yaxisArray[j]],
                    col = col[j],
                    lty = plotSymbol,
                    pch = plotSymbol,
                    lwd = symbolSize,
                    type = type)
              
              # add the y label
              if (nYAxis == 1) { 
                if (units && !is.null(dfscen$unit[[yaxisArray[[j]]]]))
                  ylabel <- paste0(ylabel, " (", 
                                   dfscen$unit[[yaxisArray[[j]]]], ")")
                
                mtext(2, text = paste(ylabel, collapse = " "), line = 2.5)
              } else {
                mtext(2, text = paste(ylabel, collapse = " "), line = 2.5)
              }
            }
          }
          
          if (nYAxis > 1) { 
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
      })
      
      # restore par settings
      par(mfrow = mfrow, mar = mar)
      
      invisible(plots)
    },
    saveSimulationOutput = function(path = "directory", scenarioXlsx = TRUE) { 
      if (missing(path))
        path <- private$pOutputId
      
      if (!dir.exists(path)) 
        dir.create(path = paste0(path, "/"), recursive = TRUE)
      
      # save the trajectories
      if (!is.null(private$pOutTrajectory))
        write.csv(x = private$pOutTrajectory, 
                  file = paste0(path, "/outputTrajectory.csv"),
                  row.names = F)
      
      if (!is.null(private$pAuxTrajectory))
        write.csv(x = private$pAuxTrajectory, 
                  file = paste0(path, "/auxTrajectory.csv"), row.names = F)
      
      if (!is.null(private$pTimeSeriesTrajectory))
        write.csv(x = private$pTimeSeriesTrajectory, 
                  file = paste0(path, "/timeSeriesTrajectory.csv"),
                  row.names = F)
      
      # save model and scenario
      private$pModel$saveXml(paste0(path, "/", private$pModel$id, 
                                    ".xml"))
      
      if (!is.null(private$pScenario)) { 
        if (scenarioXlsx)
          private$pScenario$saveXlsx(paste0(path, "/", 
                                            private$pScenario$id, 
                                            ".xlsx"))
        else
          private$pScenario$saveXml(paste0(path, "/", 
                                           private$pScenario$id, 
                                           ".xml"))
      }
    },
    UpdateOutTraj = function(traj) {
        private$pOutTrajectory <- c(private$pOutTrajectory, traj)
    },
    UpdateAuxTraj = function(row) {
      private$pAuxTrajectory <- c(private$pAuxTrajectory, row)
    }
  ),
  active = list(
    model = function() { 
      return(private[["pModel"]])
    },
    scenario = function() { 
      return(private[["pScenario"]])
    },
    outTrajectory = function() { 
      if (length(private[["pOutTrajectory"]]) == 0)
        return(NULL)
      else if(is.vector(private$pOutTrajectory)) {
        names <- c("time", names(private$pScenario$state))
        df <- data.frame(t(matrix(private$pOutTrajectory, nrow = length(names))))
        colnames(df) <- names
        return(df)
      } 
      else
        return(private$pOutTrajectory)
      
    },
    auxTrajectory = function() { 
      if (length(private[["pAuxTrajectory"]]) == 0)
        return(NULL)
      else if(is.vector(private$pAuxTrajectory)) {
        names <- names(private$pModel$aux)
        df <- data.frame(t(matrix(private$pAuxTrajectory, nrow = length(names))))
        colnames(df) <- names
        return(df)
      } else 
        return(private$pAuxTrajectory)
    },
    timeSeriesTrajectory = function() { 
      if (length(private[["pTimeSeriesTrajectory"]]) == 0)
        return(NULL)
      return(private[["pTimeSeriesTrajectory"]])
    },
    postProcessOut = function() { 
      return(private[["pPostProcessOut"]])
    },
    diagnostics = function() { 
      if (!is.null(private[["pDiag"]])) { 
        cat(private[["pDiag"]])
        return(invisible(private[["pDiag"]]))
      } else {
        return(NULL)
      }
    }
  ),
  private = list(
    pOutputId = NULL,
    pOutTrajectory = NULL,
    pAuxTrajectory = NULL,
    pTimeSeriesTrajectory = NULL,
    pPostProcessOut = NULL,
    pModel = NULL,
    pScenario = NULL,
    pDiag = NULL
  )
)