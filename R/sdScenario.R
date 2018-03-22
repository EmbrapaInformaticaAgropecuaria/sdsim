#' Class Representation of a System Scenario
#' 
#' Represents a system scenario that can consist of state variables, 
#' inputs, constant values, parameters and switches. It also stores 
#' configurations for a simulation, e.g. the time sequence and integrator 
#' method. Together, all the variables and values constitute a system 
#' environment for a model. 
#' All the variables, except the \code{state}, accepts vectors. 
#' All the object field are active binding variables that invoke a function to 
#' read it's value or to assign a value to it (<-). 
#' 
#' To create an object use the constructor \code{\link{sdScenario}}.
#' 
#' To load a scenario from an EXCEL or XML file use the 
#' \code{\link{sdLoadScenario}} function.
#' 
#' To build a coupled scenario use the \code{\link{sdBuildCoupledScenario}} 
#' function.
#' 
#' @field scenarioId A string with the scenario identification. If missing a 
#' default timestamp will be created.
#' @field times A named list containing three elements to be passed to 
#' the \code{\link{seq}} function: from - the simulation initial time, to - the 
#' simulation final time and by - the simulation time step, increment of the 
#' sequence (e.g.list(from = 0, to = 100, by = 1)).
#' @field method The default integrator to be used in the simulations, 
#' a string ("lsoda", "lsode", "lsodes","lsodar","vode", "daspk", "euler", 
#' "rk4", "ode23", "ode45", "radau", "bdf", "bdf_d", "adams", "impAdams" or 
#' "impAdams_d").
#' 
#' When running with support to events the given method must be one of the 
#' following routines, which have root-finding capability: 
#' \code{\link[deSolve]{lsoda}}, \code{\link[deSolve]{lsode}} or
#' \code{\link[deSolve]{radau}}; If the given method is different from any of 
#' these three routines the simulator will run with the default method 
#' "lsoda". 
#' 
#' See the \code{\link[deSolve]{ode}} and the \code{\link[deSolve]{events}} 
#' details section for more information.
#' @field state A numeric list with the default initial state values for an ODE 
#' system. The state variables are used to describe the mathematical "state" of 
#' a system dynamics. The continuous rate of change of these variables 
#' is determined by the model \code{DifferentialEquations} function. All the 
#' elements in this list must be named.
#' @field constant A numeric list with the model constant variables. 
#' All the elements in the list must be named.
#' @field parameter A numeric list containing the parameters of the 
#' scenario. All the elements in this list must be named.
#' @field input A list with the input variables. It accepts any type of
#' elements. The time series variables must be present in this list. All the 
#' elements in this list must be named.
#' 
#' If any interpolation method is given in the object initialization it will be 
#' stored in the input list element 'interpolation_' and the transformed 
#' temporal functions, created with the \code{\link{sdTemporalFunction}}, will 
#' be stored in the element 'fun_'. 
#' @field switch A list with the switch variables. All the elements in 
#' this list must be named.
#' @field unit A list with the model variables units. Each element of
#' this list represents a variable (named with the variable name) and it's 
#' value is a string with the variable unit. 
#' @field description A list with the model variables descriptions. 
#' Each element of this list represents a variable (named with the variable 
#' name) and it's value is the variable description.
#' 
#' @section Public Methods Definition:  
#' \describe{
#' \item{\code{$initialize(scenarioId, times, method, state, constant, 
#' parameter, input, interpolation, switch, unit, description, 
#' timeSeriesDirectory = "")}}{
#' Class constructor. Sets the model definition fields.
#' 
#' \strong{Arguments}
#' 
#' \emph{See the Fields section above for the other arguments descriptions.}
#' \describe{
#' \item{interpolation}{A list containing the interpolation methods for any time
#' series variable present the input list. All the elements in this list must be 
#' named with the respective input time series variable name. See 
#' \code{\link{sdTemporalFunction}} for the complete list of available methods.}
#' \item{timeSeriesDirectory}{The directory where the time series inputs are 
#' stored (when passing the time series inputs via external files).}
#' }
#' }
#' 
#' \item{\code{$print()}}{Print the state, constant, parameter, input and switch
#' data.frames (with variable, value, unit, description and interpolation 
#' columns).
#' }
#' 
#' \item{\code{$addState(..., verbose = FALSE, overwrite = FALSE)}}{
#' 
#' \strong{Arguments}
#' 
#' \describe{
#'   \item{...}{named fixed numeric values. If it is a single list, it's 
#'   elements will be added instead.}
#'   \item{verbose}{Logical: If \code{TRUE} provides additional details about 
#'   the added variable. Default = \code{FALSE}.}
#'   \item{overwrite}{Logical: If \code{TRUE} overwrittes all the previous added
#'   variables with the given '...' list. Default = \code{FALSE}.}
#' }}
#' 
#' \item{\code{$addConstant(..., verbose = FALSE, overwrite = FALSE)}}{
#' 
#' \strong{Arguments}
#' 
#' \describe{
#'   \item{...}{named numeric vectors. If it is a single list, it's elements 
#'   will be added instead.}
#'   \item{verbose}{Logical: If \code{TRUE} provides additional details about 
#'   the added variable. Default = \code{FALSE}.}
#'   \item{overwrite}{Logical: If \code{TRUE} overwrittes all the previous added
#'   variables with the given '...' list. Default = \code{FALSE}.}
#' }}
#' 
#' \item{\code{$addInput(..., interpolation = NULL, verbose = FALSE, 
#' overwrite = FALSE, timeSeriesDirectory = "")}}{
#' 
#' \strong{Arguments}
#' 
#' \describe{
#'   \item{...}{named objects.
#'   If it is a single list, it's elements will be added instead.}
#'   \item{interpolation}{A list containing the interpolation methods for any 
#'   time series variable given in the '...' list. All the elements in this list 
#'   must be named with the respective time series variable name. See 
#'   \code{\link{sdTemporalFunction}} for the complete list of available 
#'   methods.}
#'   \item{verbose}{Logical: If \code{TRUE} provides additional details about 
#'   the added variable. Default = \code{FALSE}.}
#'   \item{overwrite}{Logical: If \code{TRUE} overwrittes all the previous added
#'   variables with the given '...' list. Default = \code{FALSE}.}
#'   \item{timeSeriesDirectory}{The directory where time series variables are 
#'   stored (when passing the time series inputs via external files as character
#'   file names).}
#' }}
#' 
#' \item{\code{$addParameter(..., verbose = FALSE, overwrite = FALSE)}}{
#' 
#' \strong{Arguments}
#' 
#' \describe{
#'   \item{...}{named numeric vectors. If it is a single list it's elements 
#'   will be added instead.}
#'   \item{verbose}{Logical: If \code{TRUE} provides additional details about
#'   the added variable. Default = \code{FALSE}.}
#'   \item{overwrite}{Logical: If \code{TRUE} overwrittes all the previous added
#'   variables with the given '...' list. Default = \code{FALSE}.}
#' }}
#' 
#' \item{\code{$addSwitch(..., verbose = FALSE, overwrite = FALSE)}}{
#' 
#' \strong{Arguments}
#' 
#' \describe{
#'   \item{...}{named numeric or complex or logical or character vectors. 
#'   If it is a single list it's elements will be added instead.}
#'   \item{verbose}{Logical: If \code{TRUE} provides additional details about 
#'   the added variable. Default = \code{FALSE}.}
#'   \item{overwrite}{Logical: If \code{TRUE} overwrittes all the previous added
#'   variables with the given '...' list. Default = \code{FALSE}.}
#' }}
#' 
#' \item{\code{$addUnit(..., verbose = FALSE, overwrite = FALSE)}}{
#' 
#' \strong{Arguments}
#' 
#' \describe{
#'   \item{...}{named character vectors. 
#'   If it is a single list it's elements will be added instead.}
#'   \item{verbose}{Logical: If \code{TRUE} provides additional details about
#'   the added variable. Default = \code{FALSE}.}
#'   \item{overwrite}{Logical: If \code{TRUE} overwrittes all the previous added
#'   variables with the given '...' list. Default = \code{FALSE}.}
#' }}
#' 
#'  \item{\code{$addDescription(..., verbose = FALSE, overwrite = FALSE)}}{
#' 
#' \strong{Arguments}
#' 
#' \describe{
#'   \item{...}{named character vectors. 
#'   If it is a single list it's elements will be added instead.}
#'   \item{verbose}{Logical: If \code{TRUE} provides additional details about
#'   the added variable. Default = \code{FALSE}.}
#'   \item{overwrite}{Logical: If \code{TRUE} overwrittes all the previous added
#'   variables with the given '...' list. Default = \code{FALSE}.}
#' }}
#' 
#' \item{\code{$removeState(..., verbose = FALSE)}}{
#'  
#' \strong{Arguments}
#' 
#' \describe{
#'   \item{...}{Character objects containing the state variable names to be 
#'   removed. If missing all states will be removed.}
#'   \item{verbose}{Logical: If \code{TRUE} provides additional details about
#'   the removed variable. Default = \code{FALSE}.}
#' }}
#' 
#' \item{\code{$removeConstant(..., verbose = FALSE)}}{
#'  
#' \strong{Arguments}
#' 
#' \describe{
#'   \item{...}{Character objects containing the constant variable names to be 
#'   removed. If missing all constants will be removed.}
#'   \item{verbose}{Logical: If \code{TRUE} provides additional details 
#'   about the removed variable. Default = \code{FALSE}.}
#' }}
#' 
#' \item{\code{$removeParameter(..., verbose = FALSE)}}{
#'  
#' \strong{Arguments}
#' 
#' \describe{
#'   \item{...}{Character objects containing the parameter variable names to be 
#'   removed. If missing all parameters will be removed.}
#'   \item{verbose}{Logical: If \code{TRUE} provides additional details 
#'   about the removed variable. Default = \code{FALSE}.}
#' }}
#' 
#' \item{\code{$removeInput(..., verbose = FALSE)}}{
#'  
#' \strong{Arguments}
#' 
#' \describe{
#'   \item{...}{Character objects containing the input variable names to be 
#'   removed. If missing all inputs will be removed.}
#'   \item{verbose}{Logical: If \code{TRUE} provides additional details 
#'   about the removed variable. Default = \code{FALSE}.}
#' }}
#' 
#' \item{\code{$removeSwitch(..., verbose = FALSE)}}{
#'  
#' \strong{Arguments}
#' 
#' \describe{
#'   \item{...}{Character objects containing the switch variable names to be 
#'   removed. If missing all switches will be removed.}
#'   \item{verbose}{Logical: If \code{TRUE} provides additional details 
#'   about the removed variable. Default = \code{FALSE}.}
#' }}
#' 
#' \item{\code{$removeUnit(..., verbose = FALSE)}}{
#'  
#' \strong{Arguments}
#' 
#' \describe{
#'   \item{...}{Character objects containing the variable names to remove the 
#'   unit. If missing all switches will be removed.}
#'   \item{verbose}{Logical: If \code{TRUE} provides additional details 
#'   about the removed variable. Default = \code{FALSE}.}
#' }}
#' 
#' \item{\code{$removeDescription(..., verbose = FALSE)}}{
#'  
#' \strong{Arguments}
#' 
#' \describe{
#'   \item{...}{Character objects containing the variable names to remove the 
#'   description. If missing all switches will be removed.}
#'   \item{verbose}{Logical: If \code{TRUE} provides additional details 
#'   about the removed variable. Default = \code{FALSE}.}
#' }}
#' 
#' \item{\code{$setTimeSequence(from, to, by)}}{Set the simulation time 
#' sequence.
#'  
#' \strong{Arguments}
#' 
#' \describe{
#'   \item{from}{The simulation initial time; numeric.}
#'   \item{to}{The simulation final time; numeric.}
#'   \item{by}{the time step, increment of the sequence}
#' }}
#' 
#' \item{\code{$scenarioDataFrames()}}{Build the scenario variables data.frames.
#'  
#' \strong{Returned Value}
#' 
#' A list with the state, constant, parameter, input and switch
#' data.frames (with variable, value, unit, description and interpolation 
#' columns)}
#' 
#' \item{\code{$saveToXlsx(file = "Scenario.xlsx", colWidth = c(10, 10, 10, 30, 
#' 10))}}{Save the scenario variables in a EXCEL file. This format is 
#' \strong{recommended} for user modification. It will follow the format present 
#' in the EXCEL Format section of the \code{\link{sdLoadScenario}} help.
#' 
#' \strong{Arguments}
#' 
#' \describe{
#' \item{file}{A string with the file name to save to. The file extension
#' must be included in the file name, e.g. '.xlsx'.}
#' \item{colWidth}{The Excel columns width.}
#' }}
#' 
#' \item{\code{$saveToXml(file = "sdScenario.xml")}}{Save the scenario variables 
#' in a XML file. This format is \strong{not} recommended for user modification.
#' 
#' \strong{Arguments}
#' 
#' \describe{
#' \item{file}{A string with the file name to save to. The file extension
#' must be included in the file name, e.g. '.xml'.}
#' }}
#' }
#' @examples 
#' ## Let's create a scenario with two state variables, two input variables (one
#' # being a time series) and one constant
#' 
#' ## First implementation using lists:
#' 
#' # let's create a list for each type of variable (st, inp and ct)
#' # state variables
#' st <- list(s1 = 2, s2 = 5)  
#' # input variables
#' inp <- list(i1 = 10, ts1 = data.frame(Time = c(1, 5, 10), 
#'                                      Value = c(5, 10, 20)))
#' # interpoaltion method for the time series variable
#' tsInterpolation <- list(ts1 = "linear")
#' # constant variables
#' ct <- list(c1 = 0.5)
#' 
#' # let's create dummy descriptions and units for our example variables
#' descriptions <- list(s1 = "state var 1",
#'                      s2 = "state var 2",
#'                      i1 = "input var 1",
#'                      ts1 = "time series var 1",
#'                      c1 = "constant var 1")
#' units <- list(s1 = "meter",
#'               s2 = "meter / second",
#'               i1 = "1 / second",
#'               ts1 = "liters / second",
#'               c1 = "dimensionless")
#' 
#' # let's create a list for the time sequence and define the integrator method
#' times <- list(from = 0, to = 10, by = 0.5)
#' method <- "rk4"
#'               
#' # call the constructor to create a scenario from the lists
#' dummyScen <- sdScenario(scenarioId = "dummyScenario",
#'                         state = st, 
#'                         input = inp, 
#'                         interpolation = tsInterpolation,
#'                         constant = ct,
#'                         description = descriptions,
#'                         unit = units,
#'                         times = times,
#'                         method = method)
#' print(dummyScen)
#' 
#' # let's remove the input 'i1' and add it again as a function
#' dummyScen$removeInput("i1")
#' dummyScen$addInput(i1 = function(x) {x + 10})
#' print(dummyScen$input$i1(5))
#' 
#' # let's remove all the state variables and add them again by assignment
#' dummyScen$removeState()
#' dummyScen$state <- list(s1 = 2, s2 = 5) 
#' 
#' # let's add the descriptions and units again
#' dummyScen$addDescription(s1 = "state var 1",
#'                          s2 = "state var 2",
#'                          i1 = "input fun 1")
#' dummyScen$addUnit(s1 = "meter",
#'                   s2 = "meter / second",
#'                   i1 = "1 / second")
#' print(dummyScen)
#' 
#' ## Second implementation using data.frames:
#' 
#' # let's create a data.frame for each type of variable
#' # remember setting stringsAsFactor = FALSE to prevent wrong convertions
#' 
#' # state variables
#' st <- data.frame(Variable = c("s1", "s2"), 
#'                  Value = c(5,10),
#'                  Description = c("state var 1", "state var 2"),
#'                  Unit = c("meter", "meter / second"),
#'                  stringsAsFactors = FALSE)
#' 
#' # input variables
#' inp <- data.frame(Variable = c("i1", "ts1"), 
#'                  Value = c(10, "data.frame(Time = c(1, 5, 10), 
#'                                            Value = c(5, 10, 20))"),
#'                  Interpolation = c(NA, "linear"),
#'                  Description = c("input var 1", "time series var 1"),
#'                  Unit = c("1 / second", "liters / second"),
#'                  stringsAsFactors = FALSE)
#'      
#' # constant variables
#' ct <- data.frame(Variable = c("c1"), 
#'                  Value = c(0.5),
#'                  Description = c("constant var 1"),
#'                  Unit = c("dimensionless"),
#'                  stringsAsFactors = FALSE)
#'
#' # call the constructor to create a scenario from the data.frames
#' dummyScen <- sdScenario(scenarioId = "dummyScenario",
#'                         state = st, 
#'                         input = inp, 
#'                         constant = ct,
#'                         times = times,
#'                         method = method) 
#' print(dummyScen)
sdScenarioClass <- R6::R6Class(
  classname = "sdScenarioClass",
  
  public = list(
    initialize = function(scenarioId,
                          times,
                          method,
                          state,
                          constant,
                          parameter,
                          input,
                          interpolation,
                          switch,
                          unit,
                          description,
                          timeSeriesDirectory = "",
                          verbose = FALSE)
    {
      private[["pFlush"]]()
      
      # Initialize scenario ID
      if (!missing(scenarioId) && !is.null(scenarioId))
        self$scenarioId <- scenarioId
      else
        self$scenarioId <- NULL
      scenarioId <- private$pscenarioId
      
      # Initialize state
      if (!missing(state) && !is.null(state) && length(state) > 0)
      {
        if (is.list(state) || is.numeric(state))
          self$addState(state, verbose = verbose)
        else
          sdScenarioMsg$initialize1(scenarioId, "state")
      }
      
      # Initialize constant
      if (!missing(constant) && !is.null(constant) && length(constant) > 0)
      {
        if (is.list(constant) || is.numeric(constant))
          self$addConstant(constant, verbose = verbose)
        else
          sdScenarioMsg$initialize1(scenarioId, "constant")
      }
      
      # Initialize input and time series
      if (!missing(input) && !is.null(input) && length(input) > 0)
      {
        if (is.list(input))
          self$addInput(input, verbose = verbose, interpolation = interpolation,
                        timeSeriesDirectory = timeSeriesDirectory)
        else
          sdScenarioMsg$initialize2(scenarioId, "input")
      }
      
      # Initialize parameter
      if (!missing(parameter) && !is.null(parameter) && length(parameter) > 0)
      {
        if (is.list(parameter) || is.numeric(parameter))
          self$addParameter(parameter, verbose = verbose)
        else
          sdScenarioMsg$initialize1(scenarioId, "parameter")
      }
      
      # Initialize switch
      if (!missing(switch) && !is.null(switch) && length(switch) > 0)
      {
        if (is.list(switch))
          self$addSwitch(switch, verbose = verbose)
        else
          sdScenarioMsg$initialize2(scenarioId, "switch")
      }
      
      # set units list
      if (!missing(unit) && !is.null(unit) && length(unit) > 0)
      {
        if (is.list(unit) || is.character(unit))
          self$addUnit(unit, verbose = verbose)
        else
          sdScenarioMsg$initialize2(scenarioId, "unit")
      }
      
      # set descriptions list
      if (!missing(description) && !is.null(description) && 
          length(description) > 0)
      {
        if (is.list(description) || is.character(description))
          self$addDescription(description, verbose = verbose)
        else
          sdScenarioMsg$initialize2(scenarioId, "description")
      }
      
      # set simulation time sequence
      if (!missing(times) && !is.null(times) && length(times) > 0)
        self$times <- lapply(times, function(x) 
          {
            if (is.character(x))
              type.convert(x, as.is = TRUE)
            else
              x
          })
      
      if (!missing(method) && !is.null(method))
        self$method <- method
    },
    print = function()
    {
      cat("\n<sdScenario ID = ", private[["pscenarioId"]], ">\n", sep = "")
      cat(indent(paste(capture.output(self[["scenarioDataFrames"]]()), 
                       collapse =  "\n"), indent = 4))
      cat("\n")
    },
    saveToXml = function(file = "Scenario.xml")
    {
      inputs <- private[["pinput"]][!(names(private[["pinput"]]) 
                                      %in% c("interpolation_", "fun_"))]
      switches <- private[["pswitch"]]
      
      # convert any vector to character to be able to print
      inputs <- lapply(inputs, 
                       function(x) {
                         if (is.data.frame(x))
                           paste0("data.frame(Time = ", VectorToCharDef(x[[1]]), 
                                  ", Value = ", VectorToCharDef(x[[2]]), ")")
                         else if (is.function(x) || is.language(x))
                           FunToString(x)
                         else
                           x
                       })
      
      switches <- lapply(switches, 
                         function(x) {
                           if (is.data.frame(x))
                             paste0("data.frame(Time = ", 
                                    VectorToCharDef(x[[1]]), 
                                    ", Value = ", VectorToCharDef(x[[2]]), ")")
                           else if (is.function(x) || is.language(x))
                             FunToString(x)
                           else
                             x
                         })
      
      # save scenario to XML
      doc = XML::newXMLDoc()
      rootScenario <- XML::newXMLNode("sdScenario", doc = doc)
      
      lscenario <- list(scenarioId = private[["pscenarioId"]],
                        times = private[["ptimes"]],
                        method = private[["pmethod"]],
                        state = private[["pstate"]],
                        constant = private[["pconstant"]],
                        input = inputs,
                        interpolation = private[["pinput"]][["interpolation_"]],
                        parameter = private[["pparameter"]],
                        switch = switches,
                        unit = private[["punit"]],
                        description = private[["pdescription"]])
      ListToXML(rootScenario, lscenario)
      
      if (!missing(file))
        cat(XML::saveXML(doc, encoding = "UTF-8", 
                         prefix = xmlPrefix(),
                         indent = TRUE),  file = file) 
      
      return(invisible(rootScenario))
    },
    saveToXlsx = function(file = "Scenario.xlsx", 
                          colWidth = c(10, 10, 10, 30, 10))
    {
      inputData <- self[["scenarioDataFrames"]]()
      
      # Save to excel
      wb <- openxlsx::createWorkbook()
      lapply(names(inputData), function(x)
      {
        openxlsx::addWorksheet(wb, sheetName = x)
        openxlsx::writeDataTable(wb = wb, sheet = x, x = inputData[[x]])
        openxlsx::setColWidths(wb = wb, sheet = x, cols = 1:5, 
                               widths = colWidth)
      })
      
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      
      return(invisible(inputData))
    },
    setTimeSequence = function(from, to, by)
    {
      if (!missing(from) && is.numeric(from) && 
          length(from) == 1 && !is.na(from))
        private[["ptimes"]]$from <- from
      else if (!missing(from) && !is.null(from))
        sdScenarioMsg$setTimeSequence(private$pscenarioId, "from")
      
      if (!missing(to) && is.numeric(to) && 
          length(to) == 1 && !is.na(to))
        private[["ptimes"]]$to <- to
      else if (!missing(to) && !is.null(to))
        sdScenarioMsg$setTimeSequence(private$pscenarioId, "to")
      
      if (!missing(by) && is.numeric(by) && 
          length(by) == 1 && !is.na(by))
        private[["ptimes"]]$by <- by
      else if (!missing(by) && !is.null(by))
        sdScenarioMsg$setTimeSequence(private$pscenarioId, "by")
    },
    # Add variables to the scenario
    addState = function(..., verbose = FALSE, overwrite = FALSE)
    {
      private[["paddVar"]](unlist(list(...)), "state", checkNumeric = TRUE, 
                           verbose = verbose, overwrite = overwrite)
      invisible()
    },
    addConstant = function(..., verbose = FALSE, overwrite = FALSE)
    {
      private[["paddVar"]](list(...), "constant", checkNumeric = TRUE, 
                           verbose = verbose, overwrite = overwrite)
      invisible()
    },
    addInput = function(..., interpolation = NULL, verbose = FALSE, 
                        overwrite = FALSE,
                        timeSeriesDirectory = "")
    {
      private[["paddVar"]](list(...), "input", checkNumeric = FALSE, 
                           verbose = verbose, overwrite = overwrite)
      
      # check if the interpolation names are valid
      if (length(interpolation) > 0 && 
          (is.null(names(interpolation)) || 
           !all(names(interpolation) %in% names(private[["pinput"]]))))
      {
        sdScenarioMsg$addInput(private$pscenarioId, names(interpolation), 
                               names(private[["pinput"]]))
        
        interpolation <- interpolation[
          names(interpolation) %in% names(private[["pinput"]])]
      }
      
      # set time series funcs
      if (length(interpolation) > 0) 
      {
        # transform the time series data to temporal functions
        temporalfuns <- sdTemporalFunctionList(
          x = private[["pinput"]][names(interpolation)],
          methods = interpolation,
          timeSeriesDirectory = timeSeriesDirectory,
          sep = ",", dec = ".", header = TRUE)
        
        # update input list with the successful transformed time series
        private[["pinput"]][["interpolation_"]][names(temporalfuns)] <- 
          interpolation[names(temporalfuns)]
        private[["pinput"]][["fun_"]][names(temporalfuns)] <- temporalfuns
      }
      invisible()
    },
    addParameter = function(..., verbose = FALSE, overwrite = FALSE)
    {
      private[["paddVar"]](list(...), "parameter", checkNumeric = TRUE, 
                          verbose = verbose, overwrite = overwrite)
      invisible()
    },
    addSwitch = function(..., verbose = FALSE, overwrite = FALSE)
    {
      private[["paddVar"]](list(...), "switch", checkNumeric = FALSE, 
                           verbose = verbose, overwrite = overwrite)
      invisible()
    },
    addDescription = function(..., verbose = FALSE, overwrite = FALSE)
    {
      varList <- list(...)
      
      private[["paddVar"]](varList, "description", checkNumeric = FALSE, 
                          verbose = verbose, sortVars = TRUE, 
                          overwrite = overwrite)
      invisible()
    },
    addUnit = function(..., verbose = FALSE, overwrite = FALSE)
    {
      varList <- list(...)
      
      private[["paddVar"]](varList, "unit", checkNumeric = FALSE, 
                           verbose = verbose,
                          sortVars = TRUE, overwrite = overwrite)
      invisible()
    },
    # Remove variables from the scenario
    removeState = function(..., verbose = FALSE)
    {
      varList <- list(...)
      
      if (length(varList) == 0) # remove all variables
        private[["premoveVar"]](names(private$pstate), "state", 
                                verbose = verbose)
      else # remove the given variables
        private[["premoveVar"]](varList, "state", verbose = verbose)
      invisible()
    },
    removeConstant = function(..., verbose = FALSE)
    {
      varList <- list(...)
      
      if (length(varList) == 0) # remove all variables
        private[["premoveVar"]](names(private$pconstant), "constant", 
                                verbose = verbose)
      else # remove the given variables
        private[["premoveVar"]](varList, "constant", verbose = verbose)
      invisible()
    },
    removeInput = function(..., verbose = FALSE)
    {
      varList <- list(...)
      
      if (length(varList) == 0) # remove all variables
        private[["premoveVar"]](names(private$pinput), "input", 
                                verbose = verbose)
      else # remove the given variables
        private[["premoveVar"]](varList, "input", verbose = verbose)
      invisible()
    },
    removeParameter = function(..., verbose = FALSE)
    {
      varList <- list(...)
      
      if (length(varList) == 0) # remove all variables
        private[["premoveVar"]](names(private$pparameter), "parameter", 
                                verbose = verbose)
      else # remove the given variables
        private[["premoveVar"]](varList, "parameter", verbose = verbose)
      invisible()
    },
    removeSwitch = function(..., verbose = FALSE)
    {
      varList <- list(...)
      
      if (length(varList) == 0) # remove all variables
        private[["premoveVar"]](names(private$pswitch), "switch", 
                                verbose = verbose)
      else # remove the given variables
        private[["premoveVar"]](varList, "switch", verbose = verbose)
      invisible()
    },
    removeDescription = function(..., verbose = FALSE)
    {
      varList <- list(...)
      
      if (length(varList) == 0) # remove all variables
        private[["premoveVar"]](names(private$pdescription), "description", 
                                verbose = verbose)
      else # remove the given variables
        private[["premoveVar"]](varList, "description", verbose = verbose)
      invisible()
    },
    removeUnit = function(..., verbose = FALSE)
    {
      varList <- list(...)
      
      if (length(varList) == 0) # remove all variables
        private[["premoveVar"]](names(private$punit), "unit", 
                                verbose = verbose)
      else # remove the given variables
        private[["premoveVar"]](varList, "unit", verbose = verbose)
      invisible()
    },
    scenarioDataFrames = function()
    {
      # Create list of data.frames containing variable names and their
      # respective values
      varTypes <- list("pstate", "pconstant", "pparameter", "pinput", "pswitch")
      varTypesNames <- list("state", "constant", "parameter", "input", "switch")
      
      # check which lists are not empty
      validLists <- unlist(lapply(1:length(varTypes), function(x) 
        {
          if (length(private[[varTypes[[x]]]]) > 0)
            x
        }))
      varTypes <- varTypes[validLists]
      varTypesNames <- varTypesNames[validLists]
      
      inputData <- lapply(varTypes, function(varType)
      {
        # remove the interpolations and temporal functions lists
        condRemoveColumns <- !(names(private[[varType]]) %in% 
                                 c("interpolation_", "fun_"))
        nRows <- length(private[[varType]][condRemoveColumns])
        
        if (nRows == 0)
        {
          variable <- character(0)
          value <- numeric(0)
        }
        else
        {
          variable <- names(private[[varType]][condRemoveColumns])
          # convert any vector to character to be able to print
          value <- unlist(lapply(private[[varType]][condRemoveColumns], 
           function(x) {
             if (is.data.frame(x))
               paste0("data.frame(Time = ", VectorToCharDef(x[[1]]), 
                      ", Value = ", VectorToCharDef(x[[2]]), ")")
             else if (length(x) > 1)
               paste0("c(", paste0(x, collapse = ","), ")")
             else if (is.function(x) || is.language(x))
               FunToString(x)
              else
                x
            }))
        }
        
        varData <- data.frame(Variable = variable, 
                              Value = value,
                              Unit = character(nRows),
                              Description = character(nRows),
                              row.names = NULL, stringsAsFactors = FALSE)
        # add the interpolation column in the input sheet
        if (varType == "pinput")
        {
          interpolation <- rep(list(""), nRows)
          if (!is.null(private[[varType]][["interpolation_"]]))
          {
            interpolation <- lapply(variable, function(var) 
            {
              if (is.null(private[[varType]][["interpolation_"]][[var]]))
                ""
              else
                private[[varType]][["interpolation_"]][[var]]
            })
          }
          varData <- data.frame(varData, 
                                Interpolation = t(as.data.frame(interpolation)), 
                                stringsAsFactors = FALSE, row.names = NULL)
        }
        
        row.names(varData) <- NULL
        return(varData)
      })
      
      # Naming data.frames
      names(inputData) <- varTypesNames
      
      # Add descriptions and units to dataframe
      for (dfNm in names(inputData))
      {
        for (varNm in inputData[[dfNm]][["Variable"]])
        {
          if (varNm %in% names(private[["pdescription"]]))
            inputData[[dfNm]][["Description"]][[which(
              inputData[[dfNm]][["Variable"]] == varNm)]] <- 
              private[["pdescription"]][[varNm]]
          
          if (varNm %in% names(private[["punit"]]))
            inputData[[dfNm]][["Unit"]][[which(
              inputData[[dfNm]][["Variable"]] == varNm)]] <- 
              private[["punit"]][[varNm]]
        }
      }
      
      # Simulation tab containing method and times values
      if (is.null(private[["pmethod"]]))
        inputData[["simulation"]] <- data.frame(
          Variable = c(names(private[["ptimes"]]), "scenarioId"), 
          Value = c(unlist(private[["ptimes"]], use.names = FALSE), 
                    private$pscenarioId), stringsAsFactors = FALSE)
      else 
        inputData[["simulation"]] <- data.frame(
          Variable = c("method", names(private[["ptimes"]]), "scenarioId"), 
          Value = c(private[["pmethod"]], 
                    unlist(private[["ptimes"]], use.names = FALSE),
                    private$pscenarioId), stringsAsFactors = FALSE)
      
      return(inputData)
    }
  ),
  active = list(
    scenarioId = function(scenarioId)
    {
      if (missing(scenarioId))
        return(private[["pscenarioId"]])
      else # set
      {
        if (is.character(scenarioId))
          private[["pscenarioId"]] <- scenarioId
        else
        {
          scenarioId <- paste("scenario", Sys.Date())
          sdScenarioMsg$scenarioId(scenarioId)
          
          private[["pscenarioId"]] <- scenarioId
        }
      } 
    },
    state = function(varList)
    {
      if (missing(varList))
        return(private[["pstate"]])
      else if (length(varList) == 0)
        self$removeState()
      else # set
        self$addState(varList, overwrite = TRUE)
    },
    constant = function(varList)
    {
      if (missing(varList))
        return(private[["pconstant"]])
      else if (length(varList) == 0)
        self$removeConstant()
      else # set
        self$addConstant(varList, overwrite = TRUE)
    },
    input = function(varList)
    {
      if (missing(varList))
        return(private[["pinput"]])
      else if (length(varList) == 0)
        self$removeInput()
      else # set
        self$addInput(varList, overwrite = TRUE)
    },
    parameter = function(varList)
    {
      if (missing(varList))
        return(private[["pparameter"]])
      else if (length(varList) == 0)
        self$removeParameter()
      else # set
        self$addParameter(varList, overwrite = TRUE)
    },
    switch = function(varList)
    {
      if (missing(varList))
        return(private[["pswitch"]])
      else if (length(varList) == 0)
        self$removeSwitch()
      else # set
        self$addSwitch(varList, overwrite = TRUE)
    },
    method = function(method)
    {
      if (missing(method))
        return(private[["pmethod"]])
      else
      { 
        # SET method
        if (!is.character(method))
        {
          sdScenarioMsg$method1(private$pscenarioId)
          private[["pmethod"]] <- "lsoda"
        }
        
        # check if method is valid
        if (method %in% c("lsoda", "lsode", "lsodes", "lsodar", "vode", 
                          "daspk", "euler", "rk4", "ode23", "ode45", 
                          "radau", "bdf", "bdf_d", "adams", "impAdams", 
                          "impAdams_d"))
          private[["pmethod"]] <- method
        else
        {
          sdScenarioMsg$method2(private$pscenarioId)
          private[["pmethod"]] <- "lsoda"
        }
      }
    },
    times = function(times)
    {
      if (missing(times))
        return(private[["ptimes"]])
      else
      {
        if (is.vector(times) && any(names(times) %in% c("from", "to", "by")))
          self[["setTimeSequence"]](times[["from"]], 
                                    times[["to"]], 
                                    times[["by"]])
        else
          sdScenarioMsg$times(private$pscenarioId)
      }
    },
    description = function(descriptions)
    {
      if (missing(descriptions))
        return(private[["pdescription"]])
      else # set
      {
        if (is.list(descriptions))
          private[["pdescription"]] <- descriptions
        else
          sdScenarioMsg$description(private$pscenarioId, typeof(descriptions))
      }
    },
    unit = function(units)
    {
      if (missing(units))
        return(private[["punit"]])
      else # set
      {
        if (is.list(units))
          private[["punit"]] <- units
        else
          sdScenarioMsg$unit(private$pscenarioId, typeof(units))
      }
    }
  ),
  private = list(
    pscenarioId = NULL,
    pstate = list(),
    pconstant = list(),
    pparameter = list(),
    pinput = list(),
    pswitch = list(),
    pdescription = list(),
    punit = list(),
    pmethod = NULL,
    ptimes = list(),
    pFlush = function()
    {
      private[["pscenarioId"]] <- NULL
      private[["pstate"]] <- list()
      private[["pconstant"]] <- list()
      private[["pinput"]] <- list()
      private[["pparameter"]] <- list()
      private[["pswitch"]] <- list()
      private[["pdescription"]] <- list()
      private[["punit"]] <- list()
      private[["pmethod"]] <- NULL
      private[["ptimes"]] <- NULL
    },
    paddVar = function(varList = list(), varType = NULL, checkNumeric = FALSE, 
                      verbose = FALSE, sortVars = FALSE, overwrite = FALSE)
    {
      # if an unnamed list is passed, add its elements
      if (length(varList) == 1 && is.list(varList[[1]]) 
          && is.null(names(varList)))
        varList <- varList[[1]]
      
      # if is char, check if is a data structure or function definition and
      # then parse and evaluate it;
      # or convert it to the right type
      varList <- lapply(varList, FUN  = function(x) 
      {
        if (is.character(x) && length(x) == 1)
        {
          # if function, expression, vector, list, data.frame, matrix
          if (grepl(pattern = paste0("^(function|expression|c|vector|list|",
                           "data\\.frame|matrix)\\s?\\((.*\\s*)*\\)"),
                    x = x,
                    perl = TRUE)) 
          {
            tryCatch(
              {
                ex <- eval(parse(text = x))
                if (((is.function(ex) || is.language(ex)) && 
                     !(varType %in% c("input", "switch"))) || 
                    is.null(ex))
                  type.convert(x, dec = ".", numerals = "allow.loss", 
                               as.is = TRUE)
                else
                  ex
              },
              error = function(e)
              {
                type.convert(x, dec = ".", numerals = "allow.loss", 
                             as.is = TRUE)
              })
          }
          else 
            type.convert(x, dec = ".", numerals = "allow.loss", as.is = TRUE)
        }
        else
          x
      })
      
      if (is.null(names(varList)) || all(names(varList) %in% ""))
      {
        sdScenarioMsg$addVar1(private$pscenarioId)
        invisible()
      }
      
      if (overwrite)
        private[[paste0("p", varType)]] <- list()
      
      for (var in names(varList))
      {
        if (var == "")
        {
          sdScenarioMsg$addVar2(private$pscenarioId)
          next()
        }
        
        if (checkNumeric && !is.numeric(unlist(varList[[var]])))
        {
          sdScenarioMsg$addVar3(private$pscenarioId, varType, var)
        }
        else
        {
          # if (length(varList[[var]]) > 1 || is.function(varList[[var]]) 
          #     || is.language(varList[[var]]) || !is.na(varList[[var]]))
          if (!is.null(varList[[var]]))
          {
            if (verbose)
            {
              if (varType %in% c("description", "unit"))
                sdScenarioMsg$addVar4(private$pscenarioId, varType, var, 
                                      varList[[var]])
              else
                sdScenarioMsg$addVar5(private$pscenarioId, varType, var, 
                                      varList[[var]])
            }
            
            # check for duplicates
            if (var %in% names(private[[paste0("p", varType)]]))
            {
              if (verbose)
                sdScenarioMsg$addVar6(private$pscenarioId, varType, var, 
                                    varList[[var]])
              
              # if it was a ts also remove the previous interpolation and fun
              if (varType == "input")
              {
                private[["pinput"]][["interpolation_"]][[var]] <- NULL
                private[["pinput"]][["fun_"]][[var]] <- NULL
                
                if (length(private[["pinput"]][["interpolation_"]]) == 0)
                {
                  private[["pinput"]][["interpolation_"]] <- NULL
                  private[["pinput"]][["fun_"]] <- NULL
                }
              }
            }
            
            private[[paste0("p", varType)]][[var]] <- varList[[var]]
          }
          else
            sdScenarioMsg$addVar7(private$pscenarioId, varType, var)
        }
      }
      
      # sort variables by name
      if (sortVars && !is.null(names(private[[paste0("p", varType)]])))
        private[[paste0("p", varType)]] <- private[[paste0("p", varType)]][
          order(names(private[[paste0("p", varType)]]))]
      
      invisible()
    },
    premoveVar = function(varList = list(), varType = NULL, verbose = FALSE)
    {
      # If multiple one or more named lists are passed as argument
      varList <- as.list(unlist(varList))
      
      # Check if there are only characters in the parameters
      varList <- lapply(varList, function(x)
      {
        if (!is.character(x))
          sdScenarioMsg$removeVar1(private$pscenarioId, varType, typeof(x))
        else
          x
      })
      
      for (var in varList)
      { 
        if (!is.null(private[[paste0("p", varType)]][[var]]))
        {
          if (verbose)
            sdScenarioMsg$removeVar2(private$pscenarioId, varType, var)
          
          private[[paste0("p", varType)]][[var]] <- NULL
          
          # also remove units and descriptions from variables
          if (!(varType %in% c("description", "unit")))
          {
            private[["pdescription"]][[var]] <- NULL
            private[["punit"]][[var]] <- NULL
          }
          
          # in case of inputs, also remove from the interpolation and fun lists
          if (varType == "input")
          {
            private[["pinput"]][["interpolation_"]][[var]] <- NULL
            private[["pinput"]][["fun_"]][[var]] <- NULL
            
            if (length(private[["pinput"]][["interpolation_"]]) == 0)
            {
              private[["pinput"]][["interpolation_"]] <- NULL
              private[["pinput"]][["fun_"]] <- NULL
            }
          }
        }
        else
          sdScenarioMsg$removeVar3(private$pscenarioId, varType, var)       
      }
    }
  )
)
