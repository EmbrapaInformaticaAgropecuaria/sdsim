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
#' \code{ode} computation.
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
#' @field ode An R-function that computes the values of the 
#' state variables derivatives in the ODE system (the model definition) at time 
#' t.
#' 
#' It must be defined as: ode <- function(t, st, ct, par, inp, 
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
#' The return value of \code{ode} must be a list, whose first 
#' element is a vector containing the derivatives of the state variables with 
#' respect to time, and whose next elements are extra values that are 
#' computed at each time step and will be included in the simulation output. The 
#' derivatives must be specified in the same order as the state variables.
#' @field initVars (Optional) An R-function that initialize or change the 
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
#' The return value of the \code{initVars} function should be a list containing 
#' all the function arguments, except the aux equations, named in the same way, 
#' e.g. \code{return(list(st = st, ct = ct, inp = inp, par = par, sw = sw))}.
#' @field postProcess (Optional) An R-function that receives the simulation 
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
#' The return value of \code{postProcess} will be stored in the postProcess
#' field of the \code{\link{sdOutput}} simulation output object and can be 
#' anything that suits the user needs.
#' @field trigger (Optional) A numeric vector containing the times to 
#' trigger the \code{event}, or a data.frame as specified in the 
#' \code{\link[deSolve]{events}} documentation, or an R-function that becomes 
#' zero when a root occur. 
#' 
#' When a root is found, the simulation triggers an event by calling 
#' the \code{event}. If no \code{event} is defined, when a root 
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
#' @field event (Optional) An R-function that specifies the event. 
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
#' the variables in the right order. If no \code{event} is defined, when 
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
#' \item{\code{$initialize(id, description, ode, 
#' initVars, postProcess, trigger, event, aux, 
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
#'               ode = LVode,
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
                          ode, 
                          initVars,
                          postProcess, 
                          trigger,
                          event,
                          globalFunctions, 
                          auxUnits, auxDescription) { 
      funDefaultArgs <- c("t", "st", "ct", "par", "inp", "sw", "aux")

      # Create new environment for model functions
      # modelEnvironment <- new.env(parent = baseenv())
      modelEnvironment <- new.env(parent = parent.env(globalenv()))
      
      private[["pModelEnvironment"]] <- modelEnvironment
      
      # mandatory parameters
      if (!missing(id) && !is.null(id))
        self$id <- id
      else
        self$id <- NULL
      id <- private$pId
      
      if (!missing(ode) && !is.null(ode)) { 
        # TODO: do this verification in sdOde class
        # if (is.function(ode) && 
        #     all(funDefaultArgs %in% names(formals(ode))))
        #   private$pOde <- ode
        # else
        #   warning(sprintf(sdOdeModelMsg$initialize1,id))
        
        if(is.function(ode)) {
          private$pOde <- 
            sdFunctionOdeClass$new(ode)
        } else if(inherits(ode, sdFunctionOdeClass$classname)) {
          private$pOde <- ode
        } else if(inherits(ode, sdFlowOdeClass$classname)) {
          private$pOde <- ode
        } else {
          # TODO: add message; add equation list class
          stop(sprintf(""))
        }
      }
      
      if (!missing(defaultScenario) && !is.null(defaultScenario))
        self$defaultScenario <- defaultScenario
      
      # Optional parameters
      if (!missing(initVars) && !is.null(initVars)) { 
        if (is.function(initVars) && 
            all(funDefaultArgs[-1] %in% names(formals(initVars))))
          private$pInitVars <- initVars
        else
          warning(sprintf(sdOdeModelMsg$initialize2,id))
      }
      
      if (!missing(postProcess) && !is.null(postProcess)) { 
        if (is.function(postProcess) &&
            all(c("outputTrajectory", "auxTrajectory", "ct", "par", "inp", "sw") 
                %in% names(formals(postProcess))))
          private$pPostProcessVars <- postProcess
        else
          warning(sprintf(sdOdeModelMsg$initialize3,id))
      }
      
      if (!missing(trigger) && !is.null(trigger)) { 
        # convert the trigger to df or a vector
        if (is.vector(trigger) && is.character(trigger[[1]]) 
            &&  length(trigger) == 4)
          trigger <- as.data.frame(lapply(trigger, 
                                                    StringToFun))
        else if (is.character(trigger))
          trigger <- StringToFun(trigger)
        
        # convert df elements type to avoid errors
        if (is.data.frame(trigger)) { 
          trigger$var <- as.character(trigger$var)
          trigger$method <- lapply(
            as.character(trigger$method), type.convert, as.is = TRUE)
          trigger$time <- as.numeric(trigger$time)
          trigger$value <- as.numeric(trigger$value)
        }
        
        # check if the type is valid before definition
        if (is.data.frame(trigger) || is.numeric(trigger) 
            || (is.function(trigger) && 
                all(funDefaultArgs %in% names(formals(trigger)))) || 
            is.null(trigger))
          private$pTrigger <- trigger
        else
          warning(sprintf(sdOdeModelMsg$initialize4,id))
      }
      
      if (!missing(event) && !is.null(event)) { 
        if (is.function(event) && 
            all(funDefaultArgs %in% names(formals(event))))
          private$pEvent <- event
        else
          warning(sprintf(sdOdeModelMsg$initialize5,id))
      }
      
      # suppressWarnings(sdInitEquations(aux))
      if (!missing(aux) && !is.null(aux)) { # set aux
        # If aux is a list
        if (is.list(aux)) { 
          aux <- tryCatch(sdInitEquations(aux, eqName = "aux"),
                          error = function(e) { 
                            warning(sprintf(sdOdeModelMsg$initialize6,id, e))
                            return(list())
                          })
        } else if (is.character(aux) && nchar(aux) <= 255 && file.exists(aux)) { 
          # If aux is a string containing a file path
          
          aux <- paste(readLines(aux), collapse = "\n")
          aux <- tryCatch(sdInitEquations(aux, eqName = "aux"),
                          error = function(e) { 
                            sdOdeModelMsg$initialize6(id, e)
                            return(list())
                          })
        } else { 
          aux <- list()
          warning(sprintf(sdOdeModelMsg$initialize7,id))
        }
        
        # remove equations with reserved names
        if (any(names(aux) %in% sdsimReserved)) { 
          warning(sprintf(sdOdeModelMsg$initialize10, id, 
                          paste0(names(aux)[names(aux) %in% sdsimReserved], 
                                 collapse = ", ")), call. = FALSE)
          aux <- aux[!(names(aux) %in% sdsimReserved)]
        }
        
        private$pAux <- aux
      }
      
      # parent.env(modelEnvironment) <- environment(private$pOde$getOdeFunction())
      # if (is.function(private[["pOde"]]))
      #   environment(private[["pOde"]]) <- modelEnvironment
      if (is.function(private[["pInitVars"]]))
        environment(private[["pInitVars"]]) <- modelEnvironment
      if (is.function(private[["pPostProcessVars"]]))
        environment(private[["pPostProcessVars"]]) <- modelEnvironment
      if (is.function(private[["pTrigger"]]))
        environment(private[["pTrigger"]]) <- modelEnvironment
      if (is.function(private[["pEvent"]]))
        environment(private[["pEvent"]]) <- modelEnvironment
      
      # assign the global functions in the model functions environment
      if (!missing(globalFunctions) && !is.null(globalFunctions) && 
          length(globalFunctions) > 0) { 
        if (is.list(globalFunctions) && !is.null(names(globalFunctions)) && 
            all(names(globalFunctions) != "")) { # must be a named list
          remGlobalFun <- c()
          for (i in 1:length(globalFunctions)) { 
            if (!is.function(globalFunctions[[i]])) { 
              remGlobalFun <- c(remGlobalFun, i)
              warning(sprintf(sdOdeModelMsg$initialize8, id, names(globalFunctions)[[i]]))
            } else { 
              environment(globalFunctions[[i]]) <- modelEnvironment
              assign(names(globalFunctions)[[i]], globalFunctions[[i]], 
                     modelEnvironment)
            }
          }
          
          if (length(remGlobalFun) > 0)
            globalFunctions <- globalFunctions[-remGlobalFun]
          
          private$pGlobalFunctions <- globalFunctions
        } else {
          warning(sprintf(sdOdeModelMsg$initialize9,id))
        }
          
      }
      
      if (!missing(description) && !is.null(description))
        private$pDescription <- description
      
      if (!missing(auxUnits) && !is.null(auxUnits))
        private$pAuxUnits <- auxUnits
      
      if (!missing(auxDescription) && !is.null(auxDescription))
        private$pAuxDescription <- auxDescription
      
      
      
      private$flagVerify <- FALSE
    },
    print = function() { 
      # convert all the attributes to string 
      modelFuns <- list("initVars", "postProcess", 
                        "trigger", "event")
      modelStr <- lapply(modelFuns, function(f) { 
        if (is.function(private[[paste0("p", f)]]))
          FunToString(private[[paste0("p", f)]])
        else
          private[[paste0("p", f)]]
      })
      
      names(modelStr) <- unlist(modelFuns)
      nRows <- length(private$pAux)
      auxDF <- data.frame(Variable = names(private$pAux), 
                            Value = unlist(lapply(private$pAux, toString), use.names = F),
                            Unit = character(nRows),
                            Description = character(nRows),
                            row.names = NULL, stringsAsFactors = FALSE)
      
      for (varNm in auxDF[["Variable"]]) { 
        if (varNm %in% names(private$pAuxDescription))
          auxDF[["Description"]][[which(auxDF[["Variable"]] == varNm)]] <- private$pAuxDescription[[varNm]]
        
        if (varNm %in% names(private$pAuxUnits))
          auxDF[["Unit"]][[which(auxDF[["Variable"]] == varNm)]] <- private$pAuxUnits[[varNm]]
      }
      modelStr$aux <- auxDF
      modelStr$globalFunctions <- private$pGlobalFunctions
      
      # print the attributes
      cat("<",class(self)[[1]],">\n", sep = "")
      cat(indent("$id", indent = 4), sep = "\n")
      cat(indent(private$pId, indent = 4), sep = "\n")
      cat("\n")
      
      if(!is.null(private$pDescription)) {
        cat(indent("$description", indent = 4), sep = "\n")
        cat(indent(private$pDescription, indent = 4), sep = "\n")
        cat("\n")
      }
      
      if (!is.null(private$pOde)) {
        cat(indent("$ode", indent = 4), sep = "\n")
        cat(indent(capture.output(private$pOde), indent = 4), sep = "\n")
        cat("\n")
      }
      
      if (length(modelStr[["aux"]]) > 0)
        cat(indent(capture.output(modelStr["aux"]), indent = 4), sep = "\n")
      
      # remove aux and ode replicate
      for (f in names(modelStr)[!(names(modelStr) %in% 
                                  c("ode", "aux", 
                                    "globalFunctions"))]) { 
        if (!is.null(modelStr[[f]])) { 
          cat(indent(paste0("$", f), indent = 4), sep = "\n")
          cat(indent(modelStr[f], indent = 4), sep = "\n")
          cat("\n")
        }
      }
      
      if (length(modelStr[["globalFunctions"]]) > 0)
        cat(indent(capture.output(modelStr["globalFunctions"]), indent = 4), 
            sep = "\n")
      
      cat(indent("$defaultScenario", indent = 4), sep = "\n")
      cat(indent(capture.output(private$pDefaultScenario), indent = 4), 
          sep = "\n")
      cat("\n")
    },
    verifyModel = function(scenario = NULL, verbose = F) { 
      if (is.null(private$pOde))
        stop(sprintf(sdOdeModelMsg$verifyModel0, private$pId), call. = FALSE)
      
      # get the simulation scenario
      if (!is.null(private$pDefaultScenario)) { 
        # get the model scenario 
        defaultScenario <- private$pDefaultScenario$clone(deep = TRUE)
        
        # overwrite default variables with the given scenario values
        if (!is.null(scenario)) { 
          if (is.character(scenario))
            scenario <- sdLoadScenario(file = scenario)
          
          if (inherits(scenario, sdScenarioClass$classname))
            defaultScenario <- mergeScenarios(defaultScenario, scenario)
          else
            warning(sprintf(sdOdeModelMsg$verifyModel12,
                            private$pId, typeof(scenario)))
        }
      } else if (!is.null(scenario)) { 
        if (is.character(scenario))
          scenario <- sdLoadScenario(file = scenario)
        
        if (inherits(scenario, sdScenarioClass$classname)) {
          defaultScenario <- scenario
        } else { 
          sdOdeModelMsg$verifyModel12(private$pId, typeof(scenario))
          stop(sprintf(sdOdeModelMsg$verifyModel1,private$pId))
        }
      } else {
        stop(sprintf(sdOdeModelMsg$verifyModel1,private$pId))
      }
        
      # Get variables from default scenario
      st <- defaultScenario$state
      ct <- defaultScenario$constant
      par <- defaultScenario$parameter
      inp <- defaultScenario$input
      sw <- defaultScenario$switch
      times <- defaultScenario$times
      
      aux <- private$pAux
      
      if (!is.null(times) && length(unlist(times)) > 0) {
        t <- times[[1]]
      } else { 
        warning(sprintf(sdOdeModelMsg$verifyModel2,private$pId))
        t <- 0
      }
      
      # run the initialization fun
      if (!is.null(private$pInitVars)) { 
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
        stop(sprintf(sdOdeModelMsg$verifyModel13,private$pId))
      
      # compute the input time Series values for the initial time
      for (var in names(inp$fun_)) { 
        if (!is.null(inp$fun_[[var]]))
          inp[[var]] <- inp$fun_[[var]](t)
        else
          inp[[var]] <- NULL
      }
      
      auxEnv <- new.env(parent = environment())
      appendEnv(auxEnv, private$pModelEnvironment)
      
      # evaluates the auxiliary variables and update the aux list
      for (auxVar in names(aux)) { 
        aux[[auxVar]] <- tryCatch( { 
          eval(aux[[auxVar]], 
               envir = auxEnv)
        },
        error = function(e) { 
          warning(sprintf(sdOdeModelMsg$verifyModel3,private$pId, auxVar, e))
          invisible(numeric(0))
        })
        
        if (is.null(aux[[auxVar]]) || is.na(aux[[auxVar]]) ||
            length(aux[[auxVar]]) == 0 || is.infinite(aux[[auxVar]]))
          warning(sprintf(sdOdeModelMsg$verifyModel4,private$pId, auxVar, 
                          capture.output(aux[[auxVar]])))
      }
      
      #### Model Definition Validation
      ode <- private$pOde$getOdeFunction()
      
      trace_env <- new.env()
      # call the model definition and also return the auxiliary values
      res <- tryCatch( { 
        trace(ode,
              exit = function() trace_env <<- parent.frame(), 
              print = F, where = environment())
        
        ode(t = t, st = st, ct = ct, par = par, inp = inp, 
                              sw = sw, aux = aux)
      },
      error = function(e) { 
        warning(sprintf(sdOdeModelMsg$verifyModel5,private$pId, e))
        invisible(NULL)
      })
      
      lsVars <- ls(trace_env)
      # Display warnings if any variables during the Model Definition
      # execution are NULL, numeric(0), Inf or NA
      lapply(lsVars, function(x) { 
        var <- get(x, envir = trace_env)
        if (is.function(var) || is.environment(var)) { 
          var <- NULL
          # do nothing
        } else if ( (is.list(var) && length(var) > 0) ||
                  ( is.vector(var) && length(var) > 1 )) { 
          xUnlist <- unlist(var, recursive = TRUE)
          for (i in 1:length(xUnlist)) { 
            if (is.function(xUnlist[[i]]) || is.environment(xUnlist[[i]]))
              next
              # do nothing
            else if (is.null(xUnlist[[i]]))
              warning(sprintf(sdOdeModelMsg$verifyModel6,private$pId,
                              names(xUnlist)[[i]], x,"NULL"))
            else if (length(var) == 0 && is.numeric(var))
              warning(sprintf(sdOdeModelMsg$verifyModel6,private$pId,
                              names(xUnlist)[[i]], x,"numeric(0)"))
            else if (is.na(xUnlist[[i]]))
              warning(sprintf(sdOdeModelMsg$verifyModel6,private$pId,
                              names(xUnlist)[[i]], x,"NA"))
            else if (is.infinite(xUnlist[[i]]))
              warning(sprintf(sdOdeModelMsg$verifyModel6,private$pId,
                              names(xUnlist)[[i]], x,"Inf"))
          }
        } else if (x %in% c('st', 'ct', 'par', 'inp', 'sw', 'aux')) {
          # do nothing if an arg is empty
        } else if (is.null(unlist(var))) {
          warning(sprintf(sdOdeModelMsg$verifyModel7,private$pId, x, "NULL"))
        } else if (length(var) == 0 && is.numeric(var)) {
          warning(sprintf(sdOdeModelMsg$verifyModel7,private$pId, x, "numeric(0)"))
        } else if (is.na(unlist(var))) {
          warning(sprintf(sdOdeModelMsg$verifyModel7,private$pId, x, "NA"))
        } else if (is.infinite(unlist(var))) {
          warning(sprintf(sdOdeModelMsg$verifyModel7,private$pId, x, "Inf"))
        }
      })
      
      # Check the return of Model Definition contains invalid values
      if (is.list(res)) { 
        dRes <- res[[1]]
        
        if (!is.numeric(dRes))
          warning(sprintf(sdOdeModelMsg$verifyModel8,private$pId, typeof(dRes)))
        
        if (length(dRes) != length(st))
          warning(sprintf(sdOdeModelMsg$verifyModel9,private$pId, length(dRes),
                          paste0(names(dRes), collapse = ', '), length(st)))
      } else {
        warning(sprintf(sdOdeModelMsg$verifyModel10,private$pId, typeof(res)))
      }
      
      if (verbose)
        message(sprintf(sdOdeModelMsg$verifyModel11,private$pId))
      
      private$flagVerify <- TRUE
    },
    saveXml = function(file = "sdOdeModel.xml") { 
      # save model to XML
      doc <- XML::newXMLDoc()
      rootsdModel <- XML::newXMLNode(class(self)[[1]], doc = doc)
      
      if (is.function(private$pTrigger))
        trigger <- FunToString(private$pTrigger)
      else if (is.data.frame(private$pTrigger))
        trigger <- paste0(
          "data.frame(var = ", 
          VectorToCharDef(private$pTrigger$var, quote = TRUE), 
          ", time = ", 
          VectorToCharDef(private$pTrigger$time), 
          ", value = ", 
          VectorToCharDef(private$pTrigger$value), 
          ", method = ", 
          VectorToCharDef(private$pTrigger$method, quote = TRUE), 
          ")")
      else if (is.numeric(private$pTrigger))
        trigger <- VectorToCharDef(private$pTrigger)
      else
        trigger <- NULL
      
      globalFunctions <- lapply(private$pGlobalFunctions, function(x) { 
        if (is.function(x))
          return(FunToString(x))
        else
          return(x)
      })
      
      lModel <- list(id = private$pId     ,
                     description = private$pDescription,
                     initVars = FunToString(private$pInitVars),
                     postProcess = FunToString(private$pPostProcessVars),
                     trigger = trigger,
                     event = FunToString(private$pEvent),
                     aux = private$pAux,
                     auxUnits = private$pAuxUnits,
                     auxDescription = private$pAuxDescription,
                     globalFunctions = globalFunctions)
      ListToXML(rootsdModel, lModel)
      
      # add the defaultScenario XML
      if (!is.null(private$pDefaultScenario)) { 
        defaultScenarioXML <- XML::newXMLNode(
          "defaultScenario", 
          .children = list(private$pDefaultScenario$saveXml()))
        XML::addChildren(rootsdModel, kids = list(defaultScenarioXML))
      }
      
      if (!is.null(private$pOde)) { 
        odeXML <- XML::newXMLNode(
          "ode", 
          .children = list(private$pOde$saveXml()))
        XML::addChildren(rootsdModel, kids = list(odeXML))
      }
      
      if (!missing(file))
        cat(XML::saveXML(doc, encoding = "UTF-8", 
                         prefix = xmlPrefix(),
                         indent = TRUE),  file = file) 
      
      invisible(rootsdModel)
    },
    computeAux = function(t = NULL, st = NULL, ct = NULL, par = NULL, inp = NULL, sw = NULL) {
    env = new.env(globalenv())
    env$t <- t
    env$st <- st
    env$ct <- ct
    env$par <- par
    env$inp <- inp
    env$sw <- sw

    # evaluate the auxiliary variables and update the aux list
    aux <- vector("list", length = length(private$pAux))
    names(aux) <- names(private$pAux)
    auxseq <- seq_along(private$pAux)
    for (var in auxseq)
      aux[[var]] <- eval(private$pAux[[var]], envir = env)
    return(aux)
    },
    computeDiff = function(t = NULL, st = NULL, ct = NULL, par = NULL, inp = NULL, sw = NULL) { 
      # evaluate the auxiliary variables and update the aux list
      aux <- self$computeAux(t = t, st = st, ct = ct, par = par,
                        inp = inp, sw = sw)

      diff <- self$ode(t = t, st = st, ct = ct, par = par,
                           inp = inp, sw = sw, aux = aux)
      diff <- as.list(unlist(diff))
      return(diff)
    }
  ),
  active = list(
    ode = function() { 
      if(!is.null(private$pOde)) {
        ode <- private$pOde$getOdeFunction()
        parent.env(environment(ode)) <- private$pModelEnvironment
        return(ode)
        
      }
      else
        return(NULL)
    },
    initVars = function() { 
      return(private$pInitVars)
    },
    postProcess = function() { 
      return(private$pPostProcessVars)
    },
    trigger = function() { 
      return(private$pTrigger)
    },
    event = function() { 
      return(private$pEvent)
    },
    globalFunctions = function() { 
      return(private$pGlobalFunctions)
    },
    aux = function() { 
      return(private$pAux)
    },
    modelEnvironment = function() { 
      return(private$pModelEnvironment)
    }
  ),
  private = list(
    #@@ Class Private Atributes
    pOde = NULL,
    pInitVars = NULL,
    pPostProcessVars = NULL,
    pTrigger = NULL,
    pEvent = NULL,
    pGlobalFunctions = list(),
    pAux = list(),
    pModelEnvironment = NULL,
    pDescription = NULL,
    pAuxUnits = NULL,
    pAuxDescription = NULL
  )
)

