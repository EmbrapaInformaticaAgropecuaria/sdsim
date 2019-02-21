#' Class Representation of a System Static Model 
#' 
#' Represents a static (or steady-state, no state variables) model that consists 
#' of algebraic equations and a default scenario describing the system 
#' environment (variables and values). 
#' A static model calculates the system in equilibrium, and thus is 
#' time-invariant.
#' All the object field are active binding variables that invoke a function to 
#' read it's value or to assign a value to it (<-). 
#' 
#' To create an object use the constructor \code{\link{sdStaticModel}}.
#' 
#' To load a model from a XML file use the \code{\link{sdLoadModel}} function.
#' 
#' To simulate a model in different scenarios use the \code{\link{sdSimulate}}
#' function.
#' 
#' If the static model do not have time series input variables the result of the 
#' algebraic equations will always be constant in time during simulations, and 
#' therefore the simulation will only run for the initial time.
#' 
#' @field id A string with the model identification. It is coerced to a valid id 
#' following the rules described in \code{\link{sdsim-LabelingRules}}.
#' @field description A string with the model description.
#' @field defaultScenario The model default scenario, a 
#' \code{\link{sdScenarioClass}} object without state variables. 
#' It should contain all the model variables initialized with default values 
#' that ensures the model simulation. Any state variable will be removed.
#' @field equations A list with the model algebraic equations in 
#' strings or R-expressions written in R-format. 
#' 
#' They have access to the following variables: (t, ct, par, inp, sw, eq). 
#' Where \code{t} is 
#' the current time point in the integration, \code{st} is a list with the 
#' current estimate of the state variables in the ODE system, \code{ct} is a 
#' list with the model constant variables, \code{par} is a list with the model 
#' parameter variables, \code{inp} is a list with the model input variables with 
#' the time series variables evaluated for the current time step, \code{sw} is 
#' list with the model switch variables and \code{eq} is a list with the 
#' predecessors algebraic equations, following the sorted list, evaluated for 
#' the current time step. 
#' 
#' The algebraic equation are evaluated at each time step during simulations. 
#' 
#' See the function \code{\link{sdInitEquations}} to learn how this list is 
#' generated. 
#' @field InitVars (Optional) An R-function that initialize or change the 
#' initial values of the model variables before the solver call when
#' running a simulation. 
#' It can be used, for example, to compute some dependent parameter variables or 
#' the initial value of variables, using the arguments. 
#' 
#' It must be defined as: function(ct, par, inp, sw, eq). Where \code{ct} is a 
#' list with the model constant variables, \code{par} is a list with the model 
#' parameter variables, \code{inp} is a list with the model input variables, 
#' \code{sw} is a list with the model switch variables and \code{eq} is a list 
#' with the model algebraic equations in R-expression format, as defined by the 
#' user.
#' 
#' The return value of the \code{InitVars} function should be a list containing 
#' all the function arguments, except the algebraic equations, named in the same 
#' way, e.g. \code{return(list(ct = ct, inp = inp, par = par, sw = sw))}.
#' @field globalFunctions A named list of extra functions that can be executed 
#' in the scope of any other function or algebraic equation defined in the 
#' model. They can be called by their names in the list.
#' @section Public Methods Definition:  
#' \describe{
#' \item{\code{$initialize(id, description, InitVars, 
#' algebraicEquations, defaultScenario, globalFunctions)}}{
#' Class constructor. Sets the static model definition fields.
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
#' \item{\code{$saveXml(file = "sdStaticModel.xml")}}{Save the model equations
#' and functions in a XML file.
#' 
#' \strong{Arguments}
#' 
#' \describe{
#' \item{file}{A string with the file name to save the model to. The file 
#' extension must be included in the file name, e.g. '.xml'.}
#' }} 
#' }
#' @examples 
#' ## Let's create a simplified model to represent an environment capacity
#' # regulated by a population size
#' 
#' # define the parameter: the environment carrying capacity
#' parEnv <- data.frame(Variable = c("K"),
#'                         Value = c(10),
#'                         Description =c("carrying capacity"))
#'
#' # define the input: the population size
#' inpEnv <- data.frame(Variable = c("P"),
#'                      Value = c(1),
#'                      Description = c("Population size"))
#'
#' # define the algebraic equation that regulate the capacity
#' algEqEnvironment <- list(regulatingCapacity = "1 - inp$P/par$K")
#' 
#' # create the environment capacity scenario
#' envScen <- sdScenario(id = "EnvironmentCapacityScen", 
#'                       parameter = parEnv,
#'                       input = inpEnv,
#'                       times = list(from = 0, to = 200, by = 1))
#' 
#' # create the static model of an environment capacity
#' envCapacity <- sdStaticModel(id = "EnvironmentCapacity",
#'                              defaultScenario = envScen,
#'                              algebraicEquations = algEqEnvironment)
#' # validate the equations and simulate the model
#' envCapacity$verifyModel()
#' outEnvCapacity <- sdSimulate(envCapacity)
#' print(outEnvCapacity)
#' 
#' # note that static models without time series variables have constant result
#' # and therefore will only be simulated for the first time step if not coupled
sdStaticModelClass <- R6::R6Class(
  classname = "sdStaticModel",
  inherit = sdModelClass,
  public = list(
    # Class Public Atributes
    initialize = function(id,
                          description,
                          defaultScenario,
                          algebraicEquations,
                          InitVars,
                          globalFunctions) { 
      funDefaultArgs <- c("ct", "par", "inp", "sw", "eq")
      # mandatory parameters
      if (!missing(id) && !is.null(id))
        self$id <- (id)
      else
        self$id <- NULL
      id <- private$pid
      
      if (!missing(defaultScenario) && !is.null(defaultScenario))
        self$defaultScenario <- (defaultScenario) 
      
      # Optional parameters
      if (!missing(InitVars) && !is.null(InitVars)) { 
        if (is.function(InitVars) && 
            all(funDefaultArgs %in% names(formals(InitVars))))
          private$pInitVars <- InitVars
        else
          sdStaticModelMsg$initialize1(id)
      }
      
      if (!missing(algebraicEquations) && !is.null(algebraicEquations)) { # set algebraicEquations
        # If algebraicEquations is a list
        if (is.list(algebraicEquations)) { 
          algebraicEquations <- tryCatch(sdInitEquations(algebraicEquations, eqName = "eq"),
                                         error = function(e) { 
                                           sdStaticModelMsg$initialize2(id, e)
                                           return(list())
                                         })
        } else if (is.character(algebraicEquations) && nchar(algebraicEquations) <= 255 && 
                 file.exists(algebraicEquations)) { 
          # If algebraicEquations is a string containing a file path
          
          algebraicEquations <- paste(readLines(algebraicEquations), collapse = "\n")
          algebraicEquations <- tryCatch(sdInitEquations(algebraicEquations, eqName = "eq"),
                                         error = function(e) { 
                                           sdStaticModelMsg$initialize2(id, e)
                                           return(list())
                                         })
        } else { 
          algebraicEquations <- list()
          sdStaticModelMsg$initialize3(id)
        }
        # remove equations with reserved names
        if (any(names(algebraicEquations) %in% sdsimReserved)) { 
          warning(sprintf(sdStaticModelMsg$initialize6, id, 
                          paste0(
                            names(algebraicEquations)[names(algebraicEquations) 
                                                      %in% sdsimReserved], 
                            collapse = ", ")), call. = FALSE)
          algebraicEquations <- algebraicEquations[!(names(algebraicEquations) 
                                                     %in% sdsimReserved)]
        }
        
        private$palgebraicEquations <- algebraicEquations
      }
      
      # Create new environment for model functions
      modelEnvironment <- new.env(parent = baseenv())
      private$pstaticModelEnv <- modelEnvironment
      if (is.function(private[["pInitVars"]]))
        environment(private[["pInitVars"]]) <- modelEnvironment
      
      # assign the global functions in the model functions environment
      if (!missing(globalFunctions) && !is.null(globalFunctions) && 
          length(globalFunctions) > 0) { 
        if (is.list(globalFunctions) && !is.null(names(globalFunctions)) && 
            all(names(globalFunctions) != "")) { # must be a named list
          remGlobalFun <- c()
          for (i in 1:length(globalFunctions)) { 
            if (!is.function(globalFunctions[[i]])) { 
              remGlobalFun <- c(remGlobalFun, i)
              sdStaticModelMsg$initialize4(id, 
                                           names(globalFunctions)[[i]])
            } else { 
              environment(globalFunctions[[i]]) <- modelEnvironment
              assign(names(globalFunctions)[[i]], globalFunctions[[i]], 
                     modelEnvironment)
            }
          }
          
          if (length(remGlobalFun) > 0)
            globalFunctions <- globalFunctions[-remGlobalFun]
          
          private$pglobalFunctions <- globalFunctions
        } else {
          sdStaticModelMsg$initialize5(id)
        }
      }
      
      if (!missing(description) && !is.null(description))
        self$description <- (description)
      
      private$flagVerify <- FALSE
    },
    print = function() { 
      # convert all the attributes to string 
      modelStr <- list()
      modelStr$algebraicEquations <- lapply(private$palgebraicEquations, toString)
      modelStr$InitVars <- FunToString(private$pInitVars)
      modelStr$globalFunctions <- private$pglobalFunctions
      
      # print the attributes
      cat("<",class(self)[[1]],">\n", sep = "")
      cat(indent("$id", indent = 4), sep = "\n")
      cat(indent(private$pid, indent = 4), sep = "\n")
      cat("\n")
      
      cat(indent("$description", indent = 4), sep = "\n")
      cat(indent(private$pdescription, indent = 4), sep = "\n")
      cat("\n")
      
      if (length(modelStr[["algebraicEquations"]]) > 0)
        cat(indent(capture.output(modelStr["algebraicEquations"]), indent = 4), sep = "\n")
      
      if (!is.null(modelStr[["InitVars"]])) { 
        cat(indent("$InitVars", indent = 4), sep = "\n")
        cat(indent((modelStr["InitVars"]), indent = 4), sep = "\n")
        cat("\n")
      }
      
      if (length(modelStr[["globalFunctions"]]) > 0)
        cat(indent(capture.output(modelStr["globalFunctions"]), indent = 4), 
            sep = "\n")
      
      cat(indent("$defaultScenario", indent = 4), sep = "\n")
      cat(indent(capture.output(private$pdefaultScenario), indent = 4), 
          sep = "\n")
      cat("\n")
    },
    verifyModel = function(scenario = NULL, verbose = F) { 
      # run the equations and model definition validation
      if (is.null(private$pdefaultScenario))
        sdStaticModelMsg$validate0(private$pid)
      
      # get the model scenario 
      defaultScenario <- private$pdefaultScenario$clone(deep = TRUE)
      
      # overwrite default variables with the given scenario values
      if (!is.null(scenario)) { 
        if (is.character(scenario))
          scenario <- sdLoadScenario(file = scenario)
        
        if (inherits(scenario, sdScenarioClass$classname))
          defaultScenario <- mergeScenarios(defaultScenario, scenario)
        else
          sdStaticModelMsg$validate5(private$pid, 
                                     typeof(scenario))
      }
      
      # Get variables from default scenario
      ct <- defaultScenario$constant
      par <- defaultScenario$parameter
      inp <- defaultScenario$input
      sw <- defaultScenario$switch
      times <- defaultScenario$times
      
      eq <- private$palgebraicEquations
      
      if (!is.null(times) && length(unlist(times)) > 0) {
        t <- times[[1]]
      } else { 
        sdStaticModelMsg$validate1(private$pid)
        t <- 0
      }
      
      # run the initialization fun
      if (!is.null(private$pInitVars)) { 
        initVars <- private$pInitVars(ct = ct, par = par, inp = inp, sw = sw, 
                                      eq = eq)
        ct <- initVars$ct
        par <- initVars$par
        inp <- initVars$inp
        sw <- initVars$sw
      }
      
      # compute the input time Series values for the initial time
      for (var in names(inp$fun_)) { 
        if (!is.null(inp$fun_[[var]]))
          inp[[var]] <- inp$fun_[[var]](t)
        else
          inp[[var]] <- NULL
      }
      
      equationsEnv <- private$pstaticModelEnv
      parent.env(equationsEnv) <- environment()
      
      # evaluates the algebraic variables and update the equations list
      for (equationsVar in names(eq)) { 
        eq[[equationsVar]] <- tryCatch( { 
          eval(eq[[equationsVar]], 
               envir = equationsEnv)
        },
        error = function(e) { 
          sdStaticModelMsg$validate2(private$pid, equationsVar, e)
          invisible(numeric(0))
        })
        
        if (is.null(eq[[equationsVar]]) || is.na(eq[[equationsVar]]) ||
            length(eq[[equationsVar]]) == 0 || is.infinite(eq[[equationsVar]]))
          sdStaticModelMsg$validate3(private$pid, equationsVar, 
                                     eq[[equationsVar]])
      }
      
      if (verbose)
        sdStaticModelMsg$validate4(private$pid)
      
      private$flagVerify <- TRUE
    },
    saveXml = function(file = "sdStaticModel.xml") { 
      # save model to XML
      doc = XML::newXMLDoc()
      rootsdModel <- XML::newXMLNode(class(self)[[1]], doc = doc)
      
      globalFunctions <- lapply(private$pglobalFunctions, function(x) { 
        if (is.function(x))
          return(FunToString(x))
        else
          return(x)
      })
      
      lModel <- list(id = private$pid      ,
                     description = private$pdescription,
                     InitVars = FunToString(private$pInitVars),
                     algebraicEquations = private$palgebraicEquations,
                     globalFunctions = globalFunctions)
      invisible(ListToXML(rootsdModel, lModel))
      
      # add the defaultScenario XML
      if (!is.null(private$pdefaultScenario)) { 
        defaultScenarioXML <- XML::newXMLNode(
          "defaultScenario", 
          .children = list(private$pdefaultScenario$saveXml()))
        XML::addChildren(rootsdModel, kids = list(defaultScenarioXML))
      }
      
      if (!missing(file))
        cat(XML::saveXML(doc, encoding = "UTF-8", 
                         prefix = xmlPrefix(),
                         indent = T),  file = file) 
      
      invisible(rootsdModel)
    }
  ),
  active = list(
    defaultScenario = function(defaultScenario) { 
      if (missing(defaultScenario)) {
        return(private$pdefaultScenario)
      } else { # set scenario
        if (is.character(defaultScenario))
          defaultScenario <- sdLoadScenario(defaultScenario)
        
        # scenario must be a scenario object 
        if (inherits(defaultScenario, sdScenarioClass$classname)) { 
          dfScen <- defaultScenario$clone()
          if (length(dfScen$state) > 0) { 
            dfScen$removeState()
            warning(sprintf(sdStaticModelMsg$defaultscenario1, private$pid), 
                    call. = FALSE)
          }
          private$pdefaultScenario <- dfScen
          private$pdefaultScenario$id <- "Default"
          private$flagVerify <- FALSE
        } else {
          sdStaticModelMsg$defaultscenario2(private$pid)
        } 
      }      
    },
    InitVars = function() { 
      return(private$pInitVars)
    },
    GlobalFunctions = function() { 
      return(private$pglobalFunctions)
    },
    algebraicEquations = function() { 
      return(private$palgebraicEquations)
    }
  ),
  private = list(
    #@@ Class Private Atributes
    pstaticModelEnv = NULL,
    pInitVars = NULL,
    pglobalFunctions = list(),
    palgebraicEquations = list(),
    flagVerify = FALSE
  )
)