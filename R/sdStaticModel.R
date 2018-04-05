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
#' @field staticModelId A string with the model identification. Any non-word 
#' character will be removed and the result will be converted to a valid name 
#' (see \code{\link[base]{make.names}}).
#' @field staticModelDescription A string with the model description.
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
#' \item{\code{$initialize(staticModelId, staticModelDescription, InitVars, 
#' equations, defaultScenario, globalFunctions)}}{
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
#' \item{\code{$saveToXml(file = "sdStaticModel.xml")}}{Save the model equations
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
#' envScen <- sdScenario(scenarioId = "EnvironmentCapacityScen", 
#'                       parameter = parEnv,
#'                       input = inpEnv,
#'                       times = list(from = 0, to = 200, by = 1))
#' 
#' # create the static model of an environment capacity
#' envCapacity <- sdStaticModel(staticModelId = "EnvironmentCapacity",
#'                              defaultScenario = envScen,
#'                              equations = algEqEnvironment)
#' # validate the equations and simulate the model
#' envCapacity$verifyModel()
#' outEnvCapacity <- sdSimulate(envCapacity)
#' print(outEnvCapacity)
#' 
#' # note that static models without time series variables have constant result
#' # and therefore will only be simulated for the first time step if not coupled
sdStaticModelClass <- R6::R6Class(
  classname = "sdStaticModelClass",
  public = list(
    # Class Public Atributes
    initialize = function(staticModelId,
                          staticModelDescription,
                          defaultScenario,
                          equations,
                          InitVars,
                          globalFunctions)
    {
      funDefaultArgs <- c("ct", "par", "inp", "sw", "eq")
      # mandatory parameters
      if (!missing(staticModelId) && !is.null(staticModelId))
        self$staticModelId <- (staticModelId)
      else
        self$staticModelId <- NULL
      staticModelId <- private$pstaticModelId
      
      if (!missing(defaultScenario) && !is.null(defaultScenario))
        self$defaultScenario <- (defaultScenario) 
      
      # Optional parameters
      if (!missing(InitVars) && !is.null(InitVars))
      {
        if (is.function(InitVars) && 
            all(funDefaultArgs %in% names(formals(InitVars))))
          private$pInitVars <- InitVars
        else
          sdStaticModelMsg$initialize1(staticModelId)
      }
      
      if (!missing(equations) && !is.null(equations)) # set equations
      {
        # If equations is a list
        if (is.list(equations))
        {
          equations <- tryCatch(sdInitEquations(equations, eqName = "eq"),
                                error = function(e)
                                {
                                  sdStaticModelMsg$initialize2(staticModelId, e)
                                  return(list())
                                })
        }
        # If equations is a string containing a file path
        else if (is.character(equations) && nchar(equations) <= 255 && 
                 file.exists(equations))
        {
          equations <- paste(readLines(equations), collapse = "\n")
          equations <- tryCatch(sdInitEquations(equations, eqName = "eq"),
                                error = function(e)
                                {
                                  sdStaticModelMsg$initialize2(staticModelId, e)
                                  return(list())
                                })
        }
        else
        {
          equations <- list()
          sdStaticModelMsg$initialize3(staticModelId)
        }
        private$pequations <- equations
      }
      
      # Create new environment for model functions
      modelEnvironment <- new.env(parent = baseenv())
      private$pstaticModelEnv <- modelEnvironment
      if (is.function(private[["pInitVars"]]))
        environment(private[["pInitVars"]]) <- modelEnvironment
      
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
              sdStaticModelMsg$initialize4(staticModelId, 
                                           names(globalFunctions)[[i]])
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
          sdStaticModelMsg$initialize5(staticModelId)
      }
      
      if (!missing(staticModelDescription) && !is.null(staticModelDescription))
        self$staticModelDescription <- (staticModelDescription)
      
      private$flagVerify <- FALSE
    },
    print = function()
    {
      # convert all the attributes to string 
      modelStr <- list()
      modelStr$equations <- lapply(private$pequations, toString)
      modelStr$InitVars <- FunToString(private$pInitVars)
      modelStr$globalFunctions <- private$pglobalFunctions
      
      # print the attributes
      cat("\n<sdStaticModel ID = ", private$pstaticModelId, ">\n", sep = "")
      cat(indent("$staticModelDescription", indent = 4), sep = "\n")
      cat(indent(private$pstaticModelDescription, indent = 4), sep = "\n")
      cat("\n")
      
      if (length(modelStr[["equations"]]) > 0)
        cat(indent(capture.output(modelStr["equations"]), indent = 4), sep = "\n")
      
      if (!is.null(modelStr[["InitVars"]]))
      {
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
    verifyModel = function(scenario = NULL, verbose = F)
    {
      # run the equations and model definition validation
      if (is.null(private$pdefaultScenario))
        sdStaticModelMsg$validate0(private$pstaticModelId, equationsVar, e)
      
      # get the model scenario 
      defaultScenario <- private$pdefaultScenario$clone(deep = TRUE)
      
      # overwrite default variables with the given scenario values
      if (!is.null(scenario))
      {
        if (is.character(scenario))
          scenario <- sdLoadScenario(file = scenario)
        
        if (inherits(scenario, "sdScenarioClass"))
        {
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
          if (!is.null(scenario$method))
            defaultScenario$method <- scenario$method
        }
        else
          sdStaticModelMsg$validate5(private$pstaticModelId, 
                                           typeof(scenario))
      }
      
      # Get variables from default scenario
      ct <- defaultScenario$constant
      par <- defaultScenario$parameter
      inp <- defaultScenario$input
      sw <- defaultScenario$switch
      times <- defaultScenario$times

      eq <- private$pequations
      
      if (!is.null(times) && length(unlist(times)) > 0)
        t <- times[[1]]
      else
      {
        sdStaticModelMsg$validate1(private$pstaticModelId)
        t <- 0
      }
      
      # run the initialization fun
      if (!is.null(private$pInitVars))
      {
        initVars <- private$pInitVars(ct = ct, par = par, inp = inp, sw = sw, 
                                      eq = eq)
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
      
      equationsEnv <- private$pstaticModelEnv
      parent.env(equationsEnv) <- environment()
      
      # evaluates the algebraic variables and update the equations list
      for (equationsVar in names(eq))
      {
        eq[[equationsVar]] <- tryCatch(
          {
            eval(eq[[equationsVar]], 
                 envir = equationsEnv)
          },
          error = function(e)
          {
            sdStaticModelMsg$validate2(private$pstaticModelId, equationsVar, e)
            return(invisible(numeric(0)))
          })
        
        if (is.null(eq[[equationsVar]]) || is.na(eq[[equationsVar]]) ||
            length(eq[[equationsVar]]) == 0 || is.infinite(eq[[equationsVar]]))
          sdStaticModelMsg$validate3(private$pstaticModelId, equationsVar, 
                                     eq[[equationsVar]])
      }
      
      if (verbose)
        sdStaticModelMsg$validate4(private$pstaticModelId)
      
      private$flagVerify <- TRUE
    },
    saveToXml = function(file = "sdStaticModel.xml")
    {
      # save model to XML
      doc = XML::newXMLDoc()
      rootsdModel <- XML::newXMLNode("sdStaticModel", doc = doc)
      
      globalFunctions <- lapply(private$pglobalFunctions, function(x)
      {
        if (is.function(x))
          return(FunToString(x))
        else
          return(x)
      })
      
      lModel <- list(staticModelId = private$pstaticModelId      ,
                     staticModelDescription = private$pstaticModelDescription,
                     InitVars = FunToString(private$pInitVars),
                     equations = private$pequations,
                     globalFunctions = globalFunctions)
      invisible(ListToXML(rootsdModel, lModel))
      
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
                         indent = T),  file = file) 
      
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
        sdStaticModelMsg$description(private$pstaticModelId)
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
        sdStaticModelMsg$unit(private$pstaticModelId)
    },
    staticModelId = function(staticModelId)
    {
      if (missing(staticModelId))
        return(private$pstaticModelId)
      else if (is.null(staticModelId))
      {
        staticModelId <- gsub("\\s", "", paste("model", Sys.Date()), 
                              perl = T)
        sdStaticModelMsg$staticModelId1(staticModelId)
      }
      else if (!is.character(staticModelId))
      {
        staticModelId <- gsub("\\s", "", paste("model", Sys.Date()), 
                              perl = T)
        sdStaticModelMsg$staticModelId2(staticModelId)
      }
      
      private[["pstaticModelId"]] <- make.names(gsub("\\s", "", 
                                                     staticModelId, 
                                          perl = T))
    },
    defaultScenario = function(defaultScenario)
    {
      if (missing(defaultScenario))
        return(private$pdefaultScenario)
      else { # set scenario
        if (is.character(defaultScenario))
          defaultScenario <- sdLoadScenario(defaultScenario)
        
        # scenario must be a scenario object 
        if (inherits(defaultScenario, "sdScenarioClass"))
        {
          dfScen <- defaultScenario$clone()
          if (length(dfScen$state) > 0)
          {
            dfScen$removeState()
            sdStaticModelMsg$defaultscenario1(private$pstaticModelId)
          }
          private$pdefaultScenario <- dfScen
          private$pdefaultScenario$scenarioId <- "Default"
          private$flagVerify <- FALSE
        }
        else 
          sdStaticModelMsg$defaultscenario2(private$pstaticModelId)
      }      
    },
    InitVars = function()
    {
      return(private$pInitVars)
    },
    GlobalFunctions = function()
    {
      return(private$pglobalFunctions)
    },
    equations = function()
    {
      return(private$pequations)
    },
    staticModelDescription = function(staticModelDescription)
    {
      if (missing(staticModelDescription))
        return(private$pstaticModelDescription)
      else {
        if (is.character(staticModelDescription))
          private$pstaticModelDescription <- staticModelDescription
        else
          sdStaticModelMsg$staticModelDescription(private$pstaticModelId)
      }
    },
    isVerified = function()
    {
      return(private$flagVerify)
    }
  ),
  private = list(
    #@@ Class Private Atributes
    pstaticModelId = NULL,
    pstaticModelEnv = NULL,
    pstaticModelDescription = NULL,
    pInitVars = NULL,
    pdefaultScenario = NULL,
    pglobalFunctions = list(),
    pequations = list(),
    flagVerify = FALSE
  ))