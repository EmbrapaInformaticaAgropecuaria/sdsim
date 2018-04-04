#' Class Representation of an Atomic System Dynamics Model 
#' 
#' Represents an atomic system dynamics model that consists of functions 
#' describing the system flows and a default scenario describing the system 
#' environment (variables and values). 
#' All the object field are active binding variables that invoke a function to 
#' read it's value or to assign a value to it (<-).
#' 
#' To create an object use the constructor \code{\link{sdModel}}.
#' 
#' To load a model from a XML file use the \code{\link{sdLoadModel}} function.
#' 
#' To simulate a model in different scenarios use the \code{\link{sdSimulate}}
#' function.
#' 
#' @field modelId A string with the model identification. Any non-word character
#' will be removed and the result will be converted to a valid name (see 
#' \code{\link[base]{make.names}}).
#' @field modelDescription A string with the model description.
#' @field defaultScenario The model default scenario, a 
#' \code{\link{sdScenarioClass}} object. It should contain all the model
#' variables initialized with default values that ensures the model simulation.
#' @field aux (Optional) A list with the model auxiliary equations in strings or 
#' R-expressions written in R-format to assist in the 
#' \code{DifferentialEquations} computation.
#' 
#' They have access to the following variables: (t, st, ct, par, inp, sw, aux). 
#' Where \code{t} is 
#' the current time point in the integration, \code{st} is a list with the 
#' current estimate of the state variables in the ODE system, \code{ct} is a 
#' list with the model constant variables, \code{par} is a list with the model 
#' parameter variables, \code{inp} is a list with the model input variables with 
#' the time series variables evaluated for the current time step, \code{sw} is 
#' list with the model switch variables and \code{aux} is a list with the 
#' predecessors auxiliary equations, following the sorted list, evaluated for 
#' the current time step. 
#' 
#' The auxiliary equations are evaluated at each time step during simulations 
#' and passed via the argument \code{aux} to any model function call.  
#' 
#' See the function \code{\link{sdInitEquations}} to learn how this list is 
#' generated.
#' @field DifferentialEquations An R-function that computes the values of the 
#' state variables derivatives in the ODE system (the model definition) at time 
#' t.
#' 
#' It must be defined as: DifferentialEquations <- function(t, st, ct, par, inp, 
#' sw, aux). 
#' Where \code{t} is the current time point in the integration, \code{st} is 
#' a list with the current estimate of the state variables in the ODE system, 
#' \code{ct} is a list with the model constant variables, \code{par} is
#' a list with the model parameter variables, \code{inp} is a list with the
#' model input variables and the time series variables evaluated for the 
#' current time step, \code{sw} is  list with the model switch variables and
#' \code{aux} is a list with the model auxiliary equations evaluated for the 
#' current time step.
#' 
#' 
#' The return value of \code{DifferentialEquations} must be a list, whose first 
#' element is a vector containing the derivatives of the state variables with 
#' respect to time, and whose next elements are extra values that are 
#' computed at each time step and will be included in the simulation output. The 
#' derivatives must be specified in the same order as the state variables.
#' @field InitVars (Optional) An R-function that initialize or change the 
#' initial state values and/or other model variables before the solver call when
#' running a simulation. 
#' It can be used, for example, to compute some dependent parameter variables or 
#' the initial state variables, using the arguments. 
#' 
#' It must be defined as: function(st, ct, par, inp, sw, aux). Where \code{st} is 
#' a list with the initial state variables values, \code{ct} is a list with the 
#' model constant variables, \code{par} is a list with the model parameter
#' variables, \code{inp} is a list with the model input variables, \code{sw} is
#' a list with the model switch variables and \code{aux} is a list with the 
#' model auxiliary equations in R-expression format, as defined by the user.
#' 
#' The return value of the \code{InitVars} function should be a list containing 
#' all the function arguments, except the aux equations, named in the same way, 
#' e.g. \code{return(list(st = st, ct = ct, inp = inp, par = par, sw = sw))}.
#' @field PostProcessVars (Optional) An R-function that receives the simulation 
#' output inside the \code{sdSimulate} function and process it to derive further 
#' conclusions. 
#' 
#' It must be defined as: function(outputTrajectory, auxTrajectory, 
#' tsTrajectory, ct, par, inp, sw). 
#' Where \code{outputTrajectory} is a data.frame with the 
#' \code{\link[deSolve]{ode}} output trajectory, 
#' \code{auxTrajectory} is a data.frame with the auxiliary equations trajectory, 
#' \code{tsTrajectory} is a data.frame with the time series variables 
#' trajectory, \code{ct} is a list with the model constant variables, \code{par} 
#' is a list with the model parameter variables, \code{inp} is a list with the 
#' model input variables and \code{sw} is a list with the model switch 
#' variables.
#' 
#' The return value of \code{PostProcessVars} will be stored in the postProcess
#' field of the \code{\link{sdOutput}} simulation output object and can be 
#' anything that suits the user needs.
#' @field RootSpecification (Optional) A numeric vector containing the times to 
#' trigger the \code{EventFunction}, or a data.frame as specified in the 
#' \code{\link[deSolve]{events}} documentation, or an R-function that becomes 
#' zero when a root occur. 
#' 
#' When a root is found, the simulation triggers an event by calling 
#' the \code{EventFunction}. If no \code{EventFunction} is defined, when a root 
#' is found the simulation stops. 
#' 
#' When specified as a function it must be defined as: function(t, st, ct, par, 
#' inp, sw, aux). 
#' Where \code{t} is the current time point in the integration, \code{st} is 
#' a list with the current estimate of the state variables in the ODE system, 
#' \code{ct} is a list with the model constant variables, \code{par} is
#' a list with the model parameter variables, \code{inp} is a list with the
#' model input variables with the time series variables evaluated for the 
#' current time step, \code{sw} is list with the model switch variables and
#' \code{aux} is a list with the model auxiliary equations evaluated for the 
#' current time step.
#' 
#' It should return a numeric vector. If any element of this vector is zero an 
#' event is trigged.
#' @field EventFunction (Optional) An R-function that specifies the event. 
#' 
#' It must be defined as: function(t, st, ct, par, inp, sw, aux). 
#' Where \code{t} is the current time point in the integration, \code{st} is 
#' a list with the current estimate of the state variables in the ODE system, 
#' \code{ct} is a list with the model constant variables, \code{par} is
#' a list with the model parameter variables, \code{inp} is a list with the
#' model input variables with the time series variables evaluated for the 
#' current time step, \code{sw} is  list with the model switch variables and
#' \code{aux} is a list with the model auxiliary equations evaluated for the 
#' current time step. 
#' 
#' It should return the state-values (some of which modified), as a vector with 
#' the variables in the right order. If no \code{EventFunction} is defined, when 
#' a root is found the simulation stops.
#' @field description A list with the model dafault scenario variables 
#' descriptions. 
#' Each element of this list represents a variable (named with the variable 
#' name) and it's value is the variable description.
#' @field unit A list with the model dafault scenario variables units. Each 
#' element of this list represents a variable (named with the variable name) 
#' and it's value is a string with the variable unit.
#' @field globalFunctions A named list of extra functions that can be executed 
#' in the scope of any other function or auxiliary equation defined in the 
#' model. They can be called using the list names.
#' @section Public Methods Definition:  
#' \describe{
#' \item{\code{$initialize(modelId, modelDescription, DifferentialEquations, 
#' InitVars, PostProcessVars, RootSpecification, EventFunction, aux, 
#' defaultScenario, globalFunctions)}}{
#' Class constructor. Sets the model definition fields.
#' 
#' \strong{Arguments}
#' 
#' \emph{See the Fields section above for the arguments descriptions.}
#' }
#' 
#' \item{\code{$print()}}{Print the object fields.}
#' 
#' \item{\code{$validateODE(scenario = NULL, verbose = F)}}{
#' Execute the model simulation first step in the default scenario or merged 
#' with a given one. Check for possible incorrect variables and warn the user.
#' 
#' \strong{Arguments}
#' 
#' \describe{
#' \item{scenario}{A \code{\link{sdScenarioClass}} object or a character string 
#' naming the sdScenario XML or EXCEL file. If missing validate the model using
#' the default scenario.}
#' \item{verbose}{Logical: If \code{TRUE} provides additional details as to what 
#' the computer is doing. Default = \code{FALSE}.}
#' }}
#'  
#' \item{\code{$saveToXml(file = "sdModel.xml")}}{Save the model functions in a 
#' XML file.
#' 
#' \strong{Arguments}
#' 
#' \describe{
#' \item{file}{A string with the file name to save to. The file extension
#' must be included in the file name, e.g. '.xml'.}
#' }} 
#' }
#' @examples 
#' ## HOW TO CREATE A MODEL
#' ## The Lotka-Volterra consumer-prey model
#' 
#' # parameters in a list with units and descriptions in separete lists
#' pars      <- list(rI = 0.2,
#'                   rG = 1.0,    
#'                   rM = 0.2 ,   
#'                   AE = 0.5,    
#'                   K  = 10)   
#' parsUnits <- list(rI = "1/day",    
#'                   rG = "1/day",   
#'                   rM = "1/day" ,   
#'                   AE = "dimensionless",    
#'                   K  = "mmol/m3")
#' parsDescription <- list(rI = "rate of ingestion",    
#'                         rG = "growth rate of prey",   
#'                         rM = "mortality rate of consumer" ,   
#'                         AE = "assimilation efficiency",    
#'                         K  = "carrying capacity")
#' 
#' # state variables in a data.frame with values and description
#' st <- data.frame(Variable = c("P", "C"), 
#'                  Value = c(1, 2), 
#'                  Description = c("Prey", "Consumer"))
#' 
#' # time sequence
#' times <- list(from = 0, to = 200, by = 1)
#' 
#' # auxiliary equations
#' aux <- list(IngestC = "par$rI * st$P * st$C",
#'             GrowthP = "par$rG * st$P * (1 - st$P/par$K)",
#'             "MortC <- par$rM * st$C")
#' 
#' # differential equations
#' LVode <- function(t, st, ct, par, inp, sw, aux) 
#' {
#'   dP    <- aux$GrowthP - aux$IngestC
#'   dC    <- aux$IngestC * par$AE - aux$MortC
#'   
#'   return(list(c(dP, dC)))
#' }
#' 
#' # create the scenario
#' lvscen <- sdScenario(scenarioId = "LVscen", times = times, method = "lsoda",
#'                      state = st, parameter = pars, 
#'                      unit = parsUnits, description = parsDescription)
#' 
#' # create the model object
#' lv <- sdModel(modelId = "Lotka-Volterra", defaultScenario = lvscen, 
#'               DifferentialEquations = LVode,
#'               aux = aux)
#'               
#' # validate the model ode
#' lv$validateODE(verbose = TRUE)
#' 
#' # simulate the model and plot the results
#' outlv <- sdSimulate(model = lv, storeAuxTrajectory = TRUE)
#' outlv$plot("P C", multipleYAxis = TRUE, 
#'            main = "Prey and Consumer by Lotka-Volterra")
#' outlv$saveSimulationTrajectories(path = "LV")
#' 
#' ## HOW TO LOAD A MODEL FROM THE REPOSITORY
#' ## Load the Rigid Body Model
#' rb <- sdLoadModel(file = "RigidBody", repository = TRUE)
#' print(rb)
#' 
#' # simulate the model and plot the results
#' outrb <- sdSimulate(model = rb)
#' outrb$plot("x y z")
#' 
sdModelClass <- R6::R6Class(
  classname = "sdModelClass",
  public = list(
    # Class Public Atributes
    initialize = function(modelId,
                          modelDescription,
                          defaultScenario,
                          aux,
                          DifferentialEquations, 
                          InitVars,
                          PostProcessVars, 
                          RootSpecification,
                          EventFunction,
                          globalFunctions)
    {
      funDefaultArgs <- c("t", "st", "ct", "par", "inp", "sw", "aux")
      # mandatory parameters
      if (!missing(modelId) && !is.null(modelId))
        self$modelId <- modelId
      else
        self$modelId <- NULL
      modelId <- private$pmodelId
      
      if (!missing(DifferentialEquations) && !is.null(DifferentialEquations))
      {
        if (is.function(DifferentialEquations) && 
            all(funDefaultArgs %in% names(formals(DifferentialEquations))))
          private$pDifferentialEquations <- DifferentialEquations
        else
          sdModelMsg$initialize1(modelId)
      }
      
      if (!missing(defaultScenario) && !is.null(defaultScenario))
        self$defaultScenario <- defaultScenario
      
      # Optional parameters
      if (!missing(InitVars) && !is.null(InitVars))
      {
        if (is.function(InitVars) && 
            all(funDefaultArgs[-1] %in% names(formals(InitVars))))
          private$pInitVars <- InitVars
        else
          sdModelMsg$initialize2(modelId)
      }
      
      if (!missing(PostProcessVars) && !is.null(PostProcessVars))
      {
        if (is.function(PostProcessVars) &&
            all(c("outputTrajectory", "auxTrajectory", "ct", "par", "inp", "sw") 
                %in% names(formals(PostProcessVars))))
          private$pPostProcessVars <- PostProcessVars
        else
          sdModelMsg$initialize3(modelId)
      }
      
      if (!missing(RootSpecification) && !is.null(RootSpecification))
      {
        # convert the RootSpecification to df or a vector
        if (is.vector(RootSpecification) && is.character(RootSpecification[[1]]) 
            &&  length(RootSpecification) == 4)
          RootSpecification <- as.data.frame(lapply(RootSpecification, 
                                                    StringToFun))
        else if (is.character(RootSpecification))
          RootSpecification <- StringToFun(RootSpecification)
        
        # convert df elements type to avoid errors
        if (is.data.frame(RootSpecification))
        {
          RootSpecification$var <- as.character(RootSpecification$var)
          RootSpecification$method <- lapply(
            as.character(RootSpecification$method), type.convert, as.is = TRUE)
          RootSpecification$time <- as.numeric(RootSpecification$time)
          RootSpecification$value <- as.numeric(RootSpecification$value)
        }
        
        # check if the type is valid before definition
        if (is.data.frame(RootSpecification) || is.numeric(RootSpecification) 
            || (is.function(RootSpecification) && 
                all(funDefaultArgs %in% names(formals(RootSpecification)))) || 
            is.null(RootSpecification))
          private$pRootSpecification <- RootSpecification
        else
          sdModelMsg$initialize4(modelId)
      }
      
      if (!missing(EventFunction) && !is.null(EventFunction))
      {
        if (is.function(EventFunction) && 
            all(funDefaultArgs %in% names(formals(EventFunction))))
          private$pEventFunction <- EventFunction
        else
          sdModelMsg$initialize5(modelId)
      }
      
      # suppressWarnings(sdInitEquations(aux))
      if (!missing(aux) && !is.null(aux)) # set aux
      {
        # If aux is a list
        if (is.list(aux))
        {
          aux <- tryCatch(sdInitEquations(aux, eqName = "aux"),
                          error = function(e)
                          {
                            sdModelMsg$initialize6(modelId, e)
                            return(list())
                          })
        }
        # If aux is a string containing a file path
        else if (is.character(aux) && nchar(aux) <= 255 && file.exists(aux))
        {
          aux <- paste(readLines(aux), collapse = "\n")
          aux <- tryCatch(sdInitEquations(aux, eqName = "aux"),
                          error = function(e)
                          {
                            sdModelMsg$initialize6(modelId, e)
                            return(list())
                          })
        }
        else
        {
          aux <- list()
          sdModelMsg$initialize7(modelId)
        }
        private$paux <- aux
      }
      
      # Create new environment for model functions
      modelEnvironment <- new.env(parent = baseenv())
      if (is.function(private[["pDifferentialEquations"]]))
        environment(private[["pDifferentialEquations"]]) <- modelEnvironment
      if (is.function(private[["pInitVars"]]))
        environment(private[["pInitVars"]]) <- modelEnvironment
      if (is.function(private[["pPostProcessVars"]]))
        environment(private[["pPostProcessVars"]]) <- modelEnvironment
      if (is.function(private[["pRootSpecification"]]))
        environment(private[["pRootSpecification"]]) <- modelEnvironment
      if (is.function(private[["pEventFunction"]]))
        environment(private[["pEventFunction"]]) <- modelEnvironment
      
      # assign the global functions in the model functions environment
      if (!missing(globalFunctions) && !is.null(globalFunctions) && 
          length(globalFunctions) > 0)
      {
        if (is.list(globalFunctions) && !is.null(names(globalFunctions)) && 
            all(names(globalFunctions) != "")) # must be a named list
        {
          remGlobalFun <- c()
          for (i in 1:length(globalFunctions))
          {
            if (!is.function(globalFunctions[[i]]))
            {
              remGlobalFun <- c(remGlobalFun, i)
              sdModelMsg$initialize8(modelId, names(globalFunctions)[[i]])
            } 
            else
            {
              environment(globalFunctions[[i]]) <- modelEnvironment
              assign(names(globalFunctions)[[i]], globalFunctions[[i]], 
                     modelEnvironment)
            }
          }
          
          if (length(remGlobalFun) > 0)
            globalFunctions <- globalFunctions[-remGlobalFun]
          
          private$pglobalFunctions <- globalFunctions
        }
        else
          sdModelMsg$initialize9(modelId)
      }
      
      if (!missing(modelDescription) && !is.null(modelDescription))
        self$modelDescription <- modelDescription
    },
    print = function()
    {
      # convert all the attributes to string 
      modelFuns <- list("DifferentialEquations", "InitVars", "PostProcessVars", 
                        "RootSpecification", "EventFunction")
      modelStr <- lapply(modelFuns, function(f)
      {
        if (is.function(private[[paste0("p", f)]]))
          FunToString(private[[paste0("p", f)]])
        else
          private[[paste0("p", f)]]
      })
      names(modelStr) <- unlist(modelFuns)
      modelStr$aux <- lapply(private$paux, toString)
      modelStr$globalFunctions <- private$pglobalFunctions
      
      # print the attributes
      cat("\n<sdModel ID = ", private$pmodelId, ">\n", sep = "")
      cat(indent("$modelDescription", indent = 4), sep = "\n")
      cat(indent(private$pmodelDescription, indent = 4), sep = "\n")
      cat("\n")
      
      if (!is.null(modelStr[["DifferentialEquations"]]))
      {
        cat(indent("$DifferentialEquations", indent = 4), sep = "\n")
        cat(indent(modelStr[["DifferentialEquations"]], indent = 4), sep = "\n")
        cat("\n")
      }
      if (length(modelStr[["aux"]]) > 0)
        cat(indent(capture.output(modelStr["aux"]), indent = 4), sep = "\n")
      
      # remove aux and DifferentialEquations replicate
      for (f in names(modelStr)[!(names(modelStr) %in% 
                                  c("DifferentialEquations", "aux", 
                                    "globalFunctions"))]) 
      {
        if (!is.null(modelStr[[f]]))
        {
          cat(indent(paste0("$", f), indent = 4), sep = "\n")
          cat(indent(modelStr[f], indent = 4), sep = "\n")
          cat("\n")
        }
      }
      
      if (length(modelStr[["globalFunctions"]]) > 0)
        cat(indent(capture.output(modelStr["globalFunctions"]), indent = 4), 
          sep = "\n")
      
      cat(indent("$defaultScenario", indent = 4), sep = "\n")
      cat(indent(capture.output(private$pdefaultScenario), indent = 4), 
          sep = "\n")
      cat("\n")
    },
    validateODE = function(scenario = NULL, verbose = F)
    {
      # run the aux and model definition validation
      if (is.null(private$pdefaultScenario) && 
          !is.null(private$pdefaultScenario$state) && 
          length(private$pdefaultScenario$state) > 0)
        sdModelMsg$validateODE1(private$pmodelId)
      
      if (is.null(private$pDifferentialEquations))
        sdModelMsg$validateODE0(private$pmodelId)
      
      # get the model scenario 
      defaultScenario <- private$pdefaultScenario$clone(deep = TRUE)
      
      # overwrite default variables with the given scenario values
      if (!is.null(scenario))
      {
        if (is.character(scenario))
          scenario <- sdLoadScenario(file = scenario)
        
        if (inherits(scenario, "sdScenarioClass"))
        {
          if (length(scenario$state) > 0)
            defaultScenario$addState(scenario$state, verbose = verbose)
          if (length(scenario$constant) > 0)
            defaultScenario$addConstant(scenario$constant, verbose = verbose)
          if (length(scenario$input) > 0)
            defaultScenario$addInput(
              scenario$input[!(names(scenario$input) %in% c("interpolation_", 
                                                            "fun_"))],
              interpolation = scenario$input[["interpolation_"]],
              verbose = verbose)
          if (length(scenario$parameter) > 0)
            defaultScenario$addParameter(scenario$parameter, verbose = verbose)
          if (length(scenario$switch) > 0)
            defaultScenario$addSwitch(scenario$switch, verbose = verbose)
          if (!is.null(scenario$times))
            defaultScenario$times <- scenario$times
        }
        else
          sdModelMsg$validateODE12(private$pmodelId, typeof(scenario))
      }
      
      # Get variables from default scenario
      st <- defaultScenario$state
      ct <- defaultScenario$constant
      par <- defaultScenario$parameter
      inp <- defaultScenario$input
      sw <- defaultScenario$switch
      times <- defaultScenario$times
      
      aux <- private$paux
      
      if (!is.null(times) && length(unlist(times)) > 0)
        t <- times[[1]]
      else
      {
        sdModelMsg$validateODE2(private$pmodelId)
        t <- 0
      }
      
      # run the initialization fun
      if (!is.null(private$pInitVars))
      {
        initVars <- private$pInitVars(st = st, ct = ct, par = par, inp = inp, 
                                      sw = sw, aux = aux)
        st <- initVars$st
        ct <- initVars$ct
        par <- initVars$par
        inp <- initVars$inp
        sw <- initVars$sw
      }
      
      # compute the input time Series values for the initial time
      for (var in names(inp$fun_))
      {
        if (!is.null(inp$fun_[[var]]))
          inp[[var]] <- inp$fun_[[var]](t)
        else
          inp[[var]] <- NULL
      }
      
      auxEnv <- new.env(parent = environment())
      appendEnv(auxEnv, environment(private$pDifferentialEquations))
      
      # evaluates the auxiliary variables and update the aux list
      for (auxVar in names(aux))
      {
        aux[[auxVar]] <- tryCatch(
          {
            eval(aux[[auxVar]], 
                 envir = auxEnv)
          },
          error = function(e)
          {
            sdModelMsg$validateODE3(private$pmodelId, auxVar, e)
            return(invisible(numeric(0)))
          })
        
        if (is.null(aux[[auxVar]]) || is.na(aux[[auxVar]]) ||
            length(aux[[auxVar]]) == 0 || is.infinite(aux[[auxVar]]))
          sdModelMsg$validateODE4(private$pmodelId, auxVar, 
                                  capture.output(aux[[auxVar]]))
      }
      
      #### Model Definition Validation
      DifferentialEquations <- private$pDifferentialEquations
      
      # Create new environment to store variables that will be validated
      # and set env as an environment for the function
      env <- new.env(parent = environment(DifferentialEquations))
      environment(DifferentialEquations) <- env
      
      # Create test variables in env
      lsVars <- all.vars(body(DifferentialEquations))
      for(var in lsVars)
      {
        assign(var, "\\0", envir = env)
      }
      
      # replace original function with test function
      bodyStr <- as.character(body(DifferentialEquations))
      bodyStr <- gsub("(?<!<)<-", "<<-", bodyStr, perl = TRUE)
      bodyStr <- gsub("->(?!>)", "->>", bodyStr, perl = TRUE)
      bodyStr <- paste(bodyStr[2:length(bodyStr)], collapse = "\n")
      body(DifferentialEquations) <- 
        parse(text = paste("{", bodyStr, "}", sep = "\n"))
      
      # call the model definition and also return the auxiliary values
      #res <- DifferentialEquations(t = t, st = st, ct = ct, par = par, inp = inp, 
      #                        sw = sw, aux = aux)
      res <- tryCatch(
        {
          DifferentialEquations(t = t, st = st, ct = ct, par = par, inp = inp, 
                                sw = sw, aux = aux)
        },
        error = function(e)
        {
          sdModelMsg$validateODE5(private$pmodelId, e)
          return(invisible(NULL))
        })
      
      # Display warnings if any variables during the Model Definition
      # execution are NULL, numeric(0), Inf or NA
      lapply(lsVars, function(x)
      {
        var <- get(x, envir = env)
        if (is.function(var) || is.environment(var))
        {
          var <- NULL
          # do nothing
        }
        else if ( (is.list(var) && length(var) > 0) || 
                  ( is.vector(var) && length(var) > 1 )) 
        {
          xUnlist <- unlist(var, recursive = TRUE)
          for (i in 1:length(xUnlist))
          {
            if (is.function(xUnlist[[i]]) || is.environment(xUnlist[[i]]))
            {
              next
              # do nothing
            }
            else if (is.null(xUnlist[[i]]))
              sdModelMsg$validateODE6(private$pmodelId, names(xUnlist)[[i]], x, 
                                      "NULL")
            else if (length(var) == 0 && is.numeric(var))
              sdModelMsg$validateODE6(private$pmodelId, names(xUnlist)[[i]], x, 
                                      "numeric(0)")
            else if (is.na(xUnlist[[i]]))
              sdModelMsg$validateODE6(private$pmodelId, names(xUnlist)[[i]], x, 
                                      "NA")
            else if (is.infinite(xUnlist[[i]]))
              sdModelMsg$validateODE6(private$pmodelId, names(xUnlist)[[i]], x, 
                                      "Inf")
          } 
        }
        else if (x %in% c('st', 'ct', 'par', 'inp', 'sw', 'aux'))
        { # do nothing if an arg is empty
        }
        else if (is.null(unlist(var)))
          sdModelMsg$validateODE7(private$pmodelId, x, "NULL") 
        else if (length(var) == 0 && is.numeric(var))
          sdModelMsg$validateODE7(private$pmodelId, x, "numeric(0)") 
        else if (is.na(unlist(var)))
          sdModelMsg$validateODE7(private$pmodelId, x, "NA") 
        else if (is.infinite(unlist(var)))
          sdModelMsg$validateODE7(private$pmodelId, x, "Inf") 
      })
      
      # Check the return of Model Definition contains invalid values
      if (is.list(res))
      {
        dRes <- res[[1]]
        
        if (!is.numeric(dRes))
          sdModelMsg$validateODE8(private$pmodelId, typeof(dRes)) 
        
        if (length(dRes) != length(st))
          sdModelMsg$validateODE9(private$pmodelId, dRes, length(st)) 
      }
      else
        sdModelMsg$validateODE10(private$pmodelId, typeof(res)) 
      
      if (verbose)
        sdModelMsg$validateODE11(private$pmodelId) 
    },
    saveToXml = function(file = "sdModel.xml")
    {
      # save model to XML
      doc = XML::newXMLDoc()
      rootsdModel <- XML::newXMLNode("sdModel", doc = doc)
      
      if (is.function(private$pRootSpecification))
        RootSpecification <- FunToString(private$pRootSpecification)
      else if (is.data.frame(private$pRootSpecification))
        RootSpecification <- 
        paste0("data.frame(var = ", 
               VectorToCharDef(private$pRootSpecification$var, quote = TRUE), 
               ", time = ", 
               VectorToCharDef(private$pRootSpecification$time), 
               ", value = ", 
               VectorToCharDef(private$pRootSpecification$value), 
               ", method = ", 
               VectorToCharDef(private$pRootSpecification$method, quote = TRUE), 
               ")")
      else if (is.numeric(private$pRootSpecification))
        RootSpecification <- VectorToCharDef(private$pRootSpecification)
      else
        RootSpecification <- NULL
      
      globalFunctions <- lapply(private$pglobalFunctions, function(x)
      {
        if (is.function(x))
          return(FunToString(x))
        else
          return(x)
      })
      
      lModel <- list(modelId = private$pmodelId      ,
                     modelDescription = private$pmodelDescription,
                     DifferentialEquations = FunToString(private$pDifferentialEquations),
                     InitVars = FunToString(private$pInitVars),
                     PostProcessVars = FunToString(private$pPostProcessVars),
                     RootSpecification = RootSpecification,
                     EventFunction = FunToString(private$pEventFunction),
                     aux = private$paux,
                     globalFunctions = globalFunctions)
      ListToXML(rootsdModel, lModel)
      
      # add the defaultScenario XML
      if (!is.null(private$pdefaultScenario))
      {
        defaultScenarioXML <- private$pdefaultScenario$saveToXml()
        XML::xmlName(defaultScenarioXML) <- "defaultScenario"
        XML::addChildren(rootsdModel, kids = list(defaultScenarioXML))
      }
      
      if (!missing(file))
        cat(XML::saveXML(doc, encoding = "UTF-8", 
                         prefix = xmlPrefix(),
                         indent = TRUE),  file = file) 
      
      return(invisible(rootsdModel))
    }
  ),
  active = list(
    description = function(variableName)
    {
      "Return the descriptions list or the specified Variable description. 
      Show a message if the description was not found."
      if (!is.null(private$pdefaultScenario))
      {
        if (missing(variableName))
          return(private$pdefaultScenario$description)
      }
      else
        sdModelMsg$description(private$pmodelId)
    },
    unit = function(variableName)
    {
      "Return the units list or the specified Variable unit" 
      if (!is.null(private$pdefaultScenario))
      {
        if (missing(variableName))
          return(private$pdefaultScenario$unit)
      }
      else
        sdModelMsg$unit(private$pmodelId)
    },
    modelId = function(modelId)
    {
      if (missing(modelId))
        return(private$pmodelId)
      else if (is.null(modelId))
      {
        modelId <- gsub("\\s", "", paste("model", Sys.Date()), perl = TRUE)
        sdModelMsg$modelId1(modelId)
      }
      else if (!is.character(modelId))
      {
        modelId <- gsub("\\s", "", paste("model", Sys.Date()), perl = TRUE)
        sdModelMsg$modelId2(modelId)
      }
      
      # set character ID
      private[["pmodelId"]] <- make.names(gsub("\\s", "", modelId, 
                                               perl = TRUE))
    },
    DifferentialEquations = function()
    {
      return(private$pDifferentialEquations)
    },
    defaultScenario = function(defaultScenario)
    {
      if (missing(defaultScenario))
        return(private$pdefaultScenario)
      else
      {
        if (is.character(defaultScenario))
          defaultScenario <- sdLoadScenario(defaultScenario)
        
        # scenario must be a scenario object 
        if (inherits(defaultScenario, "sdScenarioClass"))
        {
          private$pdefaultScenario <- defaultScenario$clone()
          private$pdefaultScenario$scenarioId <- "Default"
        }
        else 
          sdModelMsg$defaultScenario(private$pmodelId)
      }
    },
    InitVars = function()
    {
      return(private$pInitVars)
    },
    PostProcessVars = function()
    {
      return(private$pPostProcessVars)
    },
    RootSpecification = function()
    {
      return(private$pRootSpecification)
    },
    EventFunction = function()
    {
      return(private$pEventFunction)
    },
    GlobalFunctions = function()
    {
      return(private$pglobalFunctions)
    },
    aux = function()
    {
      return(private$paux)
    },
    modelDescription = function(modelDescription)
    {
      if (missing(modelDescription))
        return(private$pmodelDescription)
      else
      {
        if (is.character(modelDescription))
          private$pmodelDescription <- modelDescription
        else
          sdModelMsg$modelDescription(private$pmodelId)
      }
    }
  ),
  private = list(
    #@@ Class Private Atributes
    pmodelId = NULL,
    pmodelDescription = NULL,
    pDifferentialEquations = NULL,
    pInitVars = NULL,
    pPostProcessVars = NULL,
    pRootSpecification = NULL,
    pEventFunction = NULL,
    pdefaultScenario = NULL,
    pglobalFunctions = list(),
    paux = list()
  ))


