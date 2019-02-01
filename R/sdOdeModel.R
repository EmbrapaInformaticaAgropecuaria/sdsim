#' Class Representation of an Atomic System Dynamics Model 
#' 
#' Represents an atomic system dynamics model that consists of functions 
#' describing the system flows and a default scenario describing the system 
#' environment (variables and values). 
#' All the object field are active binding variables that invoke a function to 
#' read it's value or to assign a value to it (<-).
#' 
#' To create an object use the constructor \code{\link{sdOdeModel}}.
#' 
#' To load a model from a XML file use the \code{\link{sdLoadModel}} function.
#' 
#' To simulate a model in different scenarios use the \code{\link{sdSimulate}}
#' function.
#' 
#' @field id A string with the model identification. It is coerced to a valid id 
#' following the rules described in \code{\link{sdsim-LabelingRules}}.
#' @field description A string with the model description.
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
#' \item{\code{$initialize(id, description, DifferentialEquations, 
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
#' \item{\code{$verifyModel(scenario = NULL, verbose = F)}}{
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
#' \item{\code{$saveXml(file = "sdOdeModel.xml")}}{Save the model functions in a 
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
#' lvscen <- sdScenario(id = "LVscen", times = times, method = "lsoda",
#'                      state = st, parameter = pars, 
#'                      unit = parsUnits, description = parsDescription)
#' 
#' # create the model object
#' lv <- sdOdeModel(id = "Lotka-Volterra", defaultScenario = lvscen, 
#'               DifferentialEquations = LVode,
#'               aux = aux)
#'               
#' # validate the model ode
#' lv$verifyModel(verbose = TRUE)
#' 
#' # simulate the model and plot the results
#' outlv <- sdSimulate(model = lv, storeAuxTrajectory = TRUE)
#' outlv$plot("P C", multipleYAxis = TRUE, 
#'            main = "Prey and Consumer by Lotka-Volterra")
#' outlv$saveSimulationOutput(path = "LV")
#' 
#' ## HOW TO LOAD A MODEL FROM THE REPOSITORY
#' ## Load the Rigid Body Model
#' rb <- sdLoadModel(file = "RigidBody", repository = TRUE)
#' print(rb)
#' 
#' # simulate the model and plot the results
#' outrb <- sdSimulate(model = rb)
#' outrb$plot("x y z")
sdOdeModelClass <- R6::R6Class(
  classname = "sdOdeModel",
  inherit = sdModelClass,
  public = list(
    # Class Public Atributes
    initialize = function(id,
                          description,
                          defaultScenario,
                          aux,
                          DifferentialEquations, 
                          InitVars,
                          PostProcessVars, 
                          RootSpecification,
                          EventFunction,
                          globalFunctions)
    {
      # Create new environment for model functions
      # modelEnvironment <- new.env(parent = baseenv())
      modelEnvironment <- new.env(parent = parent.env(globalenv()))
      
      private[["pModelEnvironment"]] <- modelEnvironment
      
      funDefaultArgs <- c("t", "st", "ct", "par", "inp", "sw", "aux")
      # mandatory parameters
      if (!missing(id) && !is.null(id))
        self$id <- id
      else
        self$id <- NULL
      id <- private$pid
      
      if (!missing(DifferentialEquations) && !is.null(DifferentialEquations))
      {
        # TODO: do this verification in sdOde class
        # if (is.function(DifferentialEquations) && 
        #     all(funDefaultArgs %in% names(formals(DifferentialEquations))))
        #   private$pDifferentialEquations <- DifferentialEquations
        # else
        #   sdOdeModelMsg$initialize1(id)
        private$pDifferentialEquations <- sdOdeClass$new(DifferentialEquations, 
                                                         modelEnvironment)
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
          sdOdeModelMsg$initialize2(id)
      }
      
      if (!missing(PostProcessVars) && !is.null(PostProcessVars))
      {
        if (is.function(PostProcessVars) &&
            all(c("outputTrajectory", "auxTrajectory", "ct", "par", "inp", "sw") 
                %in% names(formals(PostProcessVars))))
          private$pPostProcessVars <- PostProcessVars
        else
          sdOdeModelMsg$initialize3(id)
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
          sdOdeModelMsg$initialize4(id)
      }
      
      if (!missing(EventFunction) && !is.null(EventFunction))
      {
        if (is.function(EventFunction) && 
            all(funDefaultArgs %in% names(formals(EventFunction))))
          private$pEventFunction <- EventFunction
        else
          sdOdeModelMsg$initialize5(id)
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
                            sdOdeModelMsg$initialize6(id, e)
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
                            sdOdeModelMsg$initialize6(id, e)
                            return(list())
                          })
        }
        else
        {
          aux <- list()
          sdOdeModelMsg$initialize7(id)
        }
        # remove equations with reserved names
        if (any(names(aux) %in% sdsimReserved))
        {
          warning(sprintf(sdOdeModelMsg$initialize10, id, 
                          paste0(names(aux)[names(aux) %in% sdsimReserved], 
                            collapse = ", ")), call. = FALSE)
          aux <- aux[!(names(aux) %in% sdsimReserved)]
        }
        
        private$paux <- aux
      }
      
      # if (is.function(private[["pDifferentialEquations"]]))
      #   environment(private[["pDifferentialEquations"]]) <- modelEnvironment
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
              sdOdeModelMsg$initialize8(id, names(globalFunctions)[[i]])
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
          sdOdeModelMsg$initialize9(id)
      }
      
      if (!missing(description) && !is.null(description))
        self$description <- description
      
      private$flagVerify <- FALSE
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
      cat("<",class(self)[[1]],">\n", sep = "")
      cat(indent("$id", indent = 4), sep = "\n")
      cat(indent(private$pid, indent = 4), sep = "\n")
      cat("\n")
      
      cat(indent("$description", indent = 4), sep = "\n")
      cat(indent(private$pdescription, indent = 4), sep = "\n")
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
    verifyModel = function(scenario = NULL, verbose = F)
    {
      if (is.null(private$pDifferentialEquations))
        stop(sprintf(sdOdeModelMsg$verifyModel0, private$pid), call. = FALSE)
      
      # get the simulation scenario
      if (!is.null(private$pdefaultScenario))
      {
        # get the model scenario 
        defaultScenario <- private$pdefaultScenario$clone(deep = TRUE)
        
        # overwrite default variables with the given scenario values
        if (!is.null(scenario))
        {
          if (is.character(scenario))
            scenario <- sdLoadScenario(file = scenario)
          
          if (inherits(scenario, sdScenarioClass$classname))
            defaultScenario <- mergeScenarios(defaultScenario, scenario)
          else
            sdOdeModelMsg$verifyModel12(private$pid, typeof(scenario))
        }
      }
      else if (!is.null(scenario))
      {
        if (is.character(scenario))
          scenario <- sdLoadScenario(file = scenario)
        
        if (inherits(scenario, sdScenarioClass$classname))
          defaultScenario <- scenario
        else
        {
          sdOdeModelMsg$verifyModel12(private$pid, typeof(scenario))
          sdOdeModelMsg$verifyModel1(private$pid)
        }
      }
      else
        sdOdeModelMsg$verifyModel1(private$pid)
      
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
        sdOdeModelMsg$verifyModel2(private$pid)
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
      
      # check if there is state variables
      if (length(st) == 0)
        sdOdeModelMsg$verifyModel13(private$pid)
      
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
            sdOdeModelMsg$verifyModel3(private$pid, auxVar, e)
            invisible(numeric(0))
          })
        
        if (is.null(aux[[auxVar]]) || is.na(aux[[auxVar]]) ||
            length(aux[[auxVar]]) == 0 || is.infinite(aux[[auxVar]]))
          sdOdeModelMsg$verifyModel4(private$pid, auxVar, 
                                  capture.output(aux[[auxVar]]))
      }
      
      #### Model Definition Validation
      DifferentialEquations <- private$pDifferentialEquations$getOdeFunction()
      
      # Create new environment to store variables that will be validated
      # and set env as an environment for the function
      env <- new.env(parent = private$pModelEnvironment)
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
      # res <- DifferentialEquations(t = t, st = st, ct = ct, par = par,  
      #                        inp = inp, sw = sw, aux = aux)
      res <- tryCatch(
        {
          DifferentialEquations(t = t, st = st, ct = ct, par = par, inp = inp, 
                                sw = sw, aux = aux)
        },
        error = function(e)
        {
          sdOdeModelMsg$verifyModel5(private$pid, e)
          invisible(NULL)
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
              sdOdeModelMsg$verifyModel6(private$pid, names(xUnlist)[[i]], x, 
                                      "NULL")
            else if (length(var) == 0 && is.numeric(var))
              sdOdeModelMsg$verifyModel6(private$pid, names(xUnlist)[[i]], x, 
                                      "numeric(0)")
            else if (is.na(xUnlist[[i]]))
              sdOdeModelMsg$verifyModel6(private$pid, names(xUnlist)[[i]], x, 
                                      "NA")
            else if (is.infinite(xUnlist[[i]]))
              sdOdeModelMsg$verifyModel6(private$pid, names(xUnlist)[[i]], x, 
                                      "Inf")
          } 
        }
        else if (x %in% c('st', 'ct', 'par', 'inp', 'sw', 'aux'))
        { # do nothing if an arg is empty
        }
        else if (is.null(unlist(var)))
          sdOdeModelMsg$verifyModel7(private$pid, x, "NULL") 
        else if (length(var) == 0 && is.numeric(var))
          sdOdeModelMsg$verifyModel7(private$pid, x, "numeric(0)") 
        else if (is.na(unlist(var)))
          sdOdeModelMsg$verifyModel7(private$pid, x, "NA") 
        else if (is.infinite(unlist(var)))
          sdOdeModelMsg$verifyModel7(private$pid, x, "Inf") 
      })
      
      # Check the return of Model Definition contains invalid values
      if (is.list(res))
      {
        dRes <- res[[1]]
        
        if (!is.numeric(dRes))
          sdOdeModelMsg$verifyModel8(private$pid, typeof(dRes)) 
        
        if (length(dRes) != length(st))
          sdOdeModelMsg$verifyModel9(private$pid, dRes, length(st)) 
      }
      else
        sdOdeModelMsg$verifyModel10(private$pid, typeof(res)) 
      
      if (verbose)
        sdOdeModelMsg$verifyModel11(private$pid) 
      
      private$flagVerify <- TRUE
    },
    saveXml = function(file = "sdOdeModel.xml")
    {
      # save model to XML
      doc = XML::newXMLDoc()
      rootsdModel <- XML::newXMLNode(class(self)[[1]], doc = doc)
      
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
      
      lModel <- list(id = private$pid     ,
                     description = private$pdescription,
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
        defaultScenarioXML <- XML::newXMLNode(
          "defaultScenario", 
          .children = list(private$pdefaultScenario$saveXml()))
        XML::addChildren(rootsdModel, kids = list(defaultScenarioXML))
      }
      
      if (!missing(file))
        cat(XML::saveXML(doc, encoding = "UTF-8", 
                         prefix = xmlPrefix(),
                         indent = TRUE),  file = file) 
      
      invisible(rootsdModel)
    }
  ),
  active = list(
    DifferentialEquations = function()
    {
      return(private$pDifferentialEquations$getOdeFunction())
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
    modelEnvironment = function()
    {
      return(private$pModelEnvironment)
    }
  ),
  private = list(
    #@@ Class Private Atributes
    pDifferentialEquations = NULL,
    pInitVars = NULL,
    pPostProcessVars = NULL,
    pRootSpecification = NULL,
    pEventFunction = NULL,
    pglobalFunctions = list(),
    paux = list(),
    pModelEnvironment = NULL
  ))


