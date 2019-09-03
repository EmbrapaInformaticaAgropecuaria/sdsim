#' Builds a Coupled Scenario
#'
#' Merge a named list of \code{\link{sdScenarioClass}} objects in to a unique 
#' coupled scenario object with all the variables named with the prefix 
#' 'ModelId.' followed by the original variable name. The 'ModelId' is the name 
#' of each element in the \code{scenarios} list and must be the same name of the 
#' component ID that will use it.
#' Use the arguments to define the coupled scenario simulation time sequence 
#' (the arguments \code{from}, \code{to} and \code{by} must be present to define 
#' the time sequence) and the integrator method to be used in the simulations 
#' (or the default 'lsoda' will be used).
#'
#' @param id A string with the coupled scenario ID. If missing a
#' default timestamp ID will be created. 
#' @param scenarios A named list of not empty \code{\link{sdScenarioClass}} 
#' objects and/or character strings with a sdScenario XML or EXCEL file name. 
#' 
#' When passing a scenario in a EXCEL file it should follow the default format 
#' described in the \code{\link{sdLoadScenario}} documentation.
#' All the scenario elements of this list must be named with the model ID that 
#' will use it.
#' @param from The starting value of the time sequence. Numeric of
#' length 1.
#' @param to The end (maximal) value of the time sequence. Numeric of length 1.
#' @param by number: increment of the time sequence.
#' @param method The integrator to be used in the simulation,
#' a string ("lsoda", "lsode", "lsodes","lsodar","vode", "daspk", "euler",
#' "rk4", "ode23", "ode45", "radau", "bdf", "bdf_d", "adams", "impAdams" or
#' "impAdams_d"). Default value is "lsoda".
#' 
#' When running with support to events the given method must
#' be one of the following routines, which have root-finding capability:
#' \code{\link[deSolve]{lsoda}}, \code{\link[deSolve]{lsode}} or
#' \code{\link[deSolve]{radau}}; If the given method is different from any of
#' these three routines the simulator will run with the default method
#' "lsoda".
#' 
#' See the \code{\link[deSolve]{ode}} and the \code{\link[deSolve]{events}}
#' details section for more information.
#' @param timeSeriesDirectory The directory where time series inputs are stored 
#' (when passing the time series inputs via external text files).
#' @param varNames logical: if \code{TRUE} the return value is a list with two
#' elements - the \code{coupledScenario} containing the built coupled
#' scenario object and the \code{varNames} containing a list of all the
#' \code{scenarios} variables names; if \code{FALSE} the return value is just
#' the coupled scenario object, the default.
#' @return Either the coupled scenario object with all variables named with 
#' the prefix 'ModelId.' followed by the original variable name or, if 
#' \code{varNames} is \code{TRUE}, a list with two
#' elements - the \code{coupledScenario} containing the built coupled
#' scenario object and the \code{varNames} containing a list of all the
#' \code{scenarios} variables names.
#' @examples
#' ## Let's build a coupled scenario for the Lotka-Volterra example model 
#' # presented in the help('sdCoupledModel')
#' 
#' # create the Prey model variables and scenario
#' stPrey <- list(P = 1)
#' parsPrey      <- list(rG = 1.0,      
#'                       K  = 10)
#' inpPrey <- list(IngestC = 0)
#' 
#' preyScen <- sdScenario(id = "preyScen",
#'                        state = stPrey,
#'                        parameter = parsPrey,
#'                        input = inpPrey)
#'                        
#' # create the Consumer model variables and scenario
#' stConsumer <- list(C = 2)
#' parsConsumer  <- list(rI = 0.2,
#'                       rM = 0.2 ,   
#'                       AE = 0.5) 
#' inpConsumer <- list(P = 0)
#' 
#' consumerScen <- sdScenario(id = "consumerScen",
#'                            state = stConsumer,
#'                            parameter = parsConsumer,
#'                            input = inpConsumer)
#'                            
#' ## Create the coupled scenario and print it
#' coupledLvScen <- sdBuildCoupledScenario(
#'   id = "LVcoupled",
#'   scenarios = c(Prey = preyScen, 
#'                 Consumer = consumerScen), 
#'   method = "lsoda",
#'   from = 0,
#'   to = 200,
#'   by = 1) 
#' print(coupledLvScen)  
#' ## this scenario can be used to simulate the coupled Lotka-Volterra Model
#' # in a different environment by setting different values for the variables 
#' # and passing it via the argument 'scenario' to the sdSimulate function           
sdBuildCoupledScenario = function(id = NULL,
                                  scenarios = NULL,
                                  from = NULL,
                                  to = NULL,
                                  by = NULL,
                                  method = c("lsoda", "lsode", "lsodes", "lsodar", 
                                             "vode", "daspk", "euler", "rk4", 
                                             "ode23", "ode45", "radau", "bdf", 
                                             "bdf_d", "adams", "impAdams", 
                                             "impAdams_d"),
                                  timeSeriesDirectory = "",
                                  varNames = FALSE) { 
  if (!is.null(from) && !is.null(to) && !is.null(by))
    times <- list(from = from, to = to, by = by)
  else
    times <- NULL
  
  componentsId <- names(scenarios)
  
  if (is.null(componentsId) || any(componentsId == ""))
    stop(sprintf(sdCoupledModelMsg$sdBuildCoupledScenario1))
  
  # build default scenario concatanating the components default scenario
  stComponents <- list()
  ctComponents <- list()
  inpComponents <- list()
  parComponents <- list()
  swComponents <- list()
  unitComponents <- list()
  descriptionComponents <- list()
  interpolComponents <- list()
  
  modelVarNames <- list()
  
  # trim the components names
  componentsId <- gsub(" ", "", componentsId, fixed = TRUE)
  for (modelId in componentsId) { 
    scenComponent <- scenarios[[modelId]]
    
    if (is.character(scenComponent))
      scenComponent <- sdLoadScenario(file = scenComponent, 
                                      timeSeriesDirectory = timeSeriesDirectory)
    
    if (!inherits(scenComponent, sdScenarioClass$classname)) { 
      warning(sprintf(sdCoupledModelMsg$sdBuildCoupledScenario2,typeof(scenComponent),modelId))
      next
    }
    
    if (length(scenComponent$state) > 0)
      stComponents[[modelId]] <- scenComponent$state
    
    if (length(scenComponent$constant) > 0)
      ctComponents[[modelId]] <- scenComponent$constant
    
    if (length(scenComponent$input) > 0)
      inpComponents[[modelId]] <- scenComponent$input[
        !(names(scenComponent$input) %in% c("interpolation_", "fun_"))]
    
    if (length(scenComponent$input$interpolation_) > 0)
      interpolComponents[[modelId]] <-
        scenComponent$input$interpolation_
    
    if (length(scenComponent$parameter) > 0)
      parComponents[[modelId]] <- scenComponent$parameter
    
    if (length(scenComponent$switch) > 0)
      swComponents[[modelId]] <- scenComponent$switch
    
    if (length(scenComponent$unit) > 0)
      unitComponents[[modelId]] <- scenComponent$unit
    
    if (length(scenComponent$description) > 0)
      descriptionComponents[[modelId]] <-
        scenComponent$description
    
    modelVarNames[[modelId]] <- c(
      names(stComponents[[modelId]]),
      names(ctComponents[[modelId]]),
      names(inpComponents[[modelId]]),
      names(parComponents[[modelId]]),
      names(swComponents[[modelId]]))
  }
  
  coupledScenario <- sdScenario(
    id = id,
    method = method[[1]],
    times = times,
    state = unlist(stComponents, recursive = FALSE),
    constant = unlist(ctComponents, recursive = FALSE),
    input = unlist(inpComponents, recursive = FALSE),
    interpolation = as.list(unlist(interpolComponents)),
    parameter = unlist(parComponents, recursive = FALSE),
    switch = unlist(swComponents, recursive = FALSE),
    unit = unlist(unitComponents, recursive = FALSE),
    description = unlist(descriptionComponents, recursive = FALSE),
    timeSeriesDirectory = timeSeriesDirectory)
  
  if (!varNames)
    return(coupledScenario)
  else
    return(list(coupledScenario = coupledScenario, varNames = modelVarNames))
}

#' Class Representation of a Coupled System Dynamics Model
#'
#' Represents a coupled system dynamics model made up of instanced
#' \code{\link{sdOdeModelClass}}, \code{\link{sdStaticModelClass}} and/or 
#' \code{\link{sdCoupledModelClass}} components and a list of connections that 
#' define the flow of information between components. 
#' The \code{connections} list determines loops of information feedback and 
#' circular causality for conceptualizing the structure of a complex system and 
#' for communicating model-based insights.
#' The complex system is solved by integrating all the coupled system components
#' simultaneously as one, updating the connections at each time step.
#' 
#' To create an object use the constructor \code{\link{sdCoupledModel}}.
#' 
#' To load a model from a XML file use the \code{\link{sdLoadModel}} function.
#' 
#' To simulate a model in different scenarios use the \code{\link{sdSimulate}}
#' function. Make sure to build the default coupled scenario before 
#' running the simulation or some computation time will be spent running the 
#' \code{\link{sdBuildCoupledScenario}} function to build it.
#' 
#' The object fields not declared in this documentation are automatically
#' generated by the \code{$buildCoupledModel} method to be used by the package
#' during simulations, e.g. the lists with all the components functions, 
#' the vectors representing the connections (eqConnections and stConnections) 
#' and the list with the components variables indexes (indexComponents).
#'
#' @field id A string with the coupled model identification. It is coerced to a 
#' valid id following the rules described in 
#' \code{\link{sdsim-LabelingRules}}.
#' @field description A string with the coupled model description.
#' @field components A list of \code{\link{sdOdeModelClass}}, 
#' \code{\link{sdStaticModelClass}} and/or \code{\link{sdCoupledModelClass}} 
#' objects. 
#' 
#' The models must have different \code{ID}'s that will be used as unique 
#' identifiers, otherwise only the last model added with the same ID will be 
#' kept.  
#' @field connections A list of vectors that specifies the connections. 
#' 
#' Each vector represents a connection, it must have 5 elements and be defined 
#' as:
#' 
#' c(ID, Component 1, Input 1, Component 2, Output 2). 
#' 
#' Where ID: is the 
#' connection identification; Component 1: is the identification of the receiver 
#' component; Input 1: is the name of the input variable from the receiver 
#' component (component 1); Component 2: is the identification of the sender 
#' component; and Output 2: is the name of the connected state variable or, 
#' auxiliary or algebraic, equation with prefix st$, aux$ or eq$, respectively, 
#' indicating the output type from the sender component (Component 2), e.g. 
#' st$<varName>, aux$<eqName> or eq$<eqName>.
#' @field defaultScenario The default coupled scenario, a 
#' \code{\link{sdScenarioClass}} object. Uses the method
#' \code{$buildCoupledModel} to automatically build it by merging the 
#' components default scenarios. See \code{\link{sdBuildCoupledScenario}} for 
#' more information about how these merge occures.
#' @section Methods Definition:
#' \describe{
#' \item{\code{$initialize(id = NULL, 
#' description = NULL, components = NULL, connections = NULL)}}{
#' Class constructor. Define the base components and connections of the coupled 
#' system dynamics model. 
#' The \code{id} is mandatory, if missing a default timestamp ID 
#' will be created.
#'
#' \strong{Arguments}
#'
#' \emph{See the Fields section above for the arguments descriptions.}
#' }
#'
#' \item{\code{$print()}}{A short print of the object fields.}
#'
#' \item{\code{$addComponent(...)}}{Add extra components to the coupled system
#' dynamics model.
#'
#' \strong{Arguments}
#'
#' \describe{
#' \item{...}{A character string with a model XML file name or a 
#' \code{\link{sdOdeModelClass}}, \code{\link{sdStaticModelClass}} and/or 
#' \code{\link{sdCoupledModelClass}} objects already initialized. The
#' models must have different \code{ID}'s to be used as unique
#' identifiers. Only one component by ID is stored.}}}
#'
#' \item{\code{$removeComponent(...)}}{Remove the specified components from the
#' coupled system dynamics model and all connections involving them.
#'
#' \strong{Arguments}
#'
#' \describe{
#' \item{...}{Character objects containing the model IDs to be removed. If 
#' missing all components will be removed.}
#' }}
#'
#' \item{\code{$addConnection(...)}}{Add extra connections to the
#' coupled system dynamics model.
#'
#' \strong{Arguments}
#'
#' \describe{
#' \item{...}{Connection vectors with 5 character elements. See more details in 
#' the connections field section above.}
#' }}
#'
#' \item{\code{$removeConnection(...)}}{Remove the specified connections
#' from the model \code{connections} list.
#'
#' \strong{Arguments}
#'
#' \describe{
#' \item{...}{Character objects containing the connections ID's to be removed.
#' If missing all the connections will be removed.}
#' }}
#'
#' \item{\code{$buildCoupledModel(from = NULL, to = NULL, by = NULL, 
#' method = NULL, timeSeriesDirectory = "")}}{Build the default coupled scenario 
#' by merging the 
#' components default scenarios (see \code{\link{sdBuildCoupledScenario}}), 
#' initialize the coupled auxiliary and/or algebraic equations, build the 
#' vectors representing the connections (\code{eqConnections} and 
#' \code{stConnections}) and the list with the components variables indexes 
#' (\code{indexComponents}). The arguments \code{from}, \code{to} and \code{by}
#' must be present to define the simulation time sequence.
#' 
#' If the components have support to events and no component defined an event
#' function the simulation will stop in the first root, otherwise the
#' corresponding event function will be trigged.
#' 
#' Use regular expressions to update the components equations with the name
#' of the coupled scenario variables.
#'
#' \strong{Arguments}
#'
#' \describe{
#' \item{from}{The starting value of the time sequence. Numeric of length 1.}
#' \item{to}{The end (maximal) value of the time sequence. Numeric of length 1.}
#' \item{by}{number: increment of the time sequence.}
#' \item{method}{The integrator to be used in the simulation,
#' a string ("lsoda", "lsode", "lsodes","lsodar","vode", "daspk", "euler",
#' "rk4", "ode23", "ode45", "radau", "bdf", "bdf_d", "adams", "impAdams" or
#' "impAdams_d"). Default value is "lsoda".
#' 
#' When running with support to events the given method must
#' be one of the following routines, which have root-finding capability:
#' \code{\link[deSolve]{lsoda}}, \code{\link[deSolve]{lsode}} or
#' \code{\link[deSolve]{radau}}; If the given method is different from any of
#' these three routines the simulator will run with the default method
#' "lsoda".
#' 
#' See the \code{\link[deSolve]{ode}} and the \code{\link[deSolve]{events}}
#' details section for more information.}
#' \item{timeSeriesDirectory}{The directory where time series inputs are stored 
#' (when passing the time series inputs via external text files).}
#' }
#' }
#' 
#' \item{\code{$isBuilt()}}{Test if the coupled model was already built and not
#' modified. Return a logical object.
#' }
#'
#' \item{\code{$verifyModel(scenario = NULL, verbose = FALSE, 
#' timeSeriesDirectory = "")}}{Execute the first step of the coupled model 
#' simulation in the default scenario or merged with a given one. 
#' Check for possible incorrect return values and variables in the components 
#' definitions and warn the user. 
#'
#' \strong{Arguments}
#'
#' \describe{
#' \item{scenario}{A coupled scenario object (see 
#' \code{\link{sdBuildCoupledScenario}}), or a list of sdScenario objects and/or 
#' character strings with a sdScenario XML or EXCEL file name - the
#' elements of this list must be named with the model ID that represent's it.
#' If missing validate the model using the default scenario.}
#' \item{verbose}{Logical: If \code{TRUE} provides additional details as to what 
#' the computer is doing. Default = \code{FALSE}.}
#' \item{timeSeriesDirectory}{The directory where time series inputs are stored 
#' (when passing the time series inputs via external text files).}
#' }}
#'
#' \item{\code{$saveXml(file = "sdCoupledModel.xml")}}{Save the components 
#' and the connections in a XML file.
#'
#' \strong{Arguments}
#'
#' \describe{
#' \item{file}{A character object naming the file to save to. The file extension
#' must be included in the file name, e.g. '.xml'.}
#' }}
#' }
#' @examples 
#' # The Lotka-Volterra consumer-prey model implemented as a coupled model with
#' # the components being the population models of a prey and a consumer and a
#' # static model for the environment capacity
#' 
#' # set the time sequence and use the default method
#' times <- list(from = 0, to = 200, by = 1)
#' 
#' # Prey model variables and ode function
#' stPrey <- list(P = 1)
#' parsPrey      <- list(rG = 1.0)
#' inpPrey <- list(ingestC = 0,
#'                 envCapacity = 1)
#' auxPrey <- list(GrowthP = "par$rG * st$P * inp$envCapacity")
#' descriptionsPrey <- list(P = "population of preys",
#'                          rG = "growth rate of prey",   
#'                          ingestC = "Consumer ingestion",
#'                          envCapacity = "available environment capacity")
#' 
#' LVodePrey <- function(t, st, ct, par, inp, sw, aux) 
#' {
#'   dP    <- aux$GrowthP - inp$ingestC
#'   
#'   return(list(c(dP)))
#' }
#' 
#' # create the component prey model
#' prey <- sdOdeModel(
#'   id = "Prey",
#'   defaultScenario = sdScenario(
#'     id = "preyScen",
#'     times = times,
#'     state = stPrey,
#'     parameter = parsPrey,
#'     input = inpPrey,
#'     description = descriptionsPrey),
#'   aux = auxPrey,
#'   ode = LVodePrey)
#' 
#' # Consumer model variables and ode function
#' stConsumer <- list(C = 2)
#' parsConsumer  <- list(rI = 0.2,
#'                       rM = 0.2 ,   
#'                       aE = 0.5) 
#' inpConsumer <- list(P = 0)
#' auxConsumer <- list(ingestC = "par$rI * inp$P * st$C",
#'                     "mortC <- par$rM * st$C")
#' descriptionsConsumer <- list(C = "population of consumer",
#'                              rM = "mortality rate of consumer", 
#'                              aE = "assimilation efficiency",
#'                              rI = "rate of ingestion",
#'                              P = "population of preys")
#' 
#' LVodeConsumer <- function(t, st, ct, par, inp, sw, aux) 
#' {
#'   dC    <- aux$ingestC * par$aE - aux$mortC
#'   
#'   return(list(c(dC)))
#' }
#' 
#' # create the component consumer model
#' consumer <- sdOdeModel(
#'   id = "Consumer",
#'   defaultScenario = sdScenario(
#'     id = "consumerScen",
#'     times = times,
#'     state = stConsumer,
#'     parameter = parsConsumer,
#'     input = inpConsumer,
#'     description = descriptionsConsumer),
#'   aux = auxConsumer,
#'   ode = LVodeConsumer)
#' 
#' # Environment model variables and algebraic equations
#' parEnv <- data.frame(Variable = c("K"),
#'                     Value = c(10),
#'                     Description =c("carrying capacity"))
#' inpEnv <- data.frame(Variable = c("P"),
#'                     Value = c(1),
#'                     Description = c("Population size"))
#' eqEnvironment <- list(regulatingCapacity = "1 - inp$P/par$K")  
#' 
#' # create the component environment capacity model
#' environmentCap <- sdStaticModel(id = "Environment",
#'                              defaultScenario = sdScenario(
#'                                id = "EnvironmentScen",
#'                                parameter = parEnv,
#'                                input = inpEnv,
#'                                times = times),
#'                              algebraicEquations = eqEnvironment)                   
#' 
#' # create the coupled model connections list 
#' # conP: inform the consumer model about the amount of preys; 
#' # conIngestC: inform the prey model about the consumer ingestion;
#' # conPEnv: inform the environment model about the amount of preys and
#' # conEnvCapacity: inform the prey model about the available environment capacity
#' lvConnections <- list(
#'   c("conP", "Consumer", "P", "Prey", "st$P"),
#'   c("conIngestC", "Prey", "ingestC", "Consumer", "aux$ingestC"),
#'   c("conPEnv", "Environment", "P", "Prey", "st$P"),
#'   c("conEnvCapacity", "Prey", "envCapacity", "Environment", "eq$regulatingCapacity"))
#' 
#' # Create the Lotka-Volterra coupled model
#' coupledLV <- sdCoupledModel(id = "LVCoupled",
#'                             components = c(prey, consumer, environmentCap),
#'                             connections = lvConnections,
#'                             description = 
#'   "Lotka-Volterra Equations implemented as a coupled model")
#' # build the coupled model and validate it
#' coupledLV$buildCoupledModel(from = 0, 
#'                             to = 200, 
#'                             by = 1)
#' coupledLV$verifyModel(verbose = TRUE)
#' 
#' # simulate the coupled model and plot the results
#' outclv <- sdSimulate(model = coupledLV)
#' outclv$plot("Prey.P Environment.regulatingCapacity", "Prey.P Consumer.C", 
#'             main = c("Coupled Prey Regulated by Environment Capacity",
#'                      "Coupled Prey and Consumer by Lotka-Volterra"),
#'             multipleYAxis = TRUE)
sdCoupledModelClass <- R6::R6Class(
  classname = "sdCoupledModel",
  inherit = sdModelClass,
  public = list(
    # Class Public Methods
    initialize = function(id = NULL,
                          description = NULL,
                          components = NULL,
                          connections = NULL) { 
      if (!missing(id) && !is.null(id))
        self$id <- id
      else
        self$id <- NULL
      
      if (!missing(components) && !is.null(components)) { 
        if (is.list(components)) { 
          for (model in components)
            self$addComponent(model)
        } else {
          self$addComponent(components)
        }
          
      }
      
      if (!missing(connections) && !is.null(connections)) { 
        if (is.list(connections)) { 
          for (con in connections)
            self$addConnection(con)
        } else {
          self$addConnection(connections)
        }
      }
      
      if (!missing(description) &&
          !is.null(description))
        self$description <- description
      
      private$pCoupledEnv <- new.env(parent = baseenv())
    },
    print = function() { 
      # print the attributes
      cat("<",class(self)[[1]],">\n", sep = "")
      cat(indent("$id", indent = 4), sep = "\n")
      cat(indent(private$pId, indent = 4), sep = "\n")
      cat("\n")
      
      cat(indent("$description", indent = 4), sep = "\n")
      cat(indent(private$pDescription, indent = 4), sep = "\n")
      cat("\n")
      
      cat(indent("$components", indent = 4), sep = "\n")
      cat(indent(private$pComponentsId, indent = 4), sep = "\n")
      cat("\n")
      
      cat(indent("$connections", indent = 4), sep = "\n")
      cat(indent(capture.output(private$pConnections), indent = 4), sep = "\n")
      cat("\n")
      
      cat(indent("$defaultScenario", indent = 4), sep = "\n")
      if (private$flagBuild)
        cat(indent(capture.output(private$pDefaultScenario), indent = 4), 
            sep = "\n")
      else
        cat(indent("Not built.", indent = 4), sep = "\n")
      cat("\n")
    },
    addComponent = function(...) { 
      components <- list(...)
      
      for (model in components) { 
        if (is.character(model))
          model <- sdLoadModel(file = model)
        
        # check if it is a valid model
        if (!inherits(model, sdModelClass$classname)) { 
          warning(sprintf(sdCoupledModelMsg$addComponent2, private$pId, 
                          typeof(model)),
                  call. = FALSE)
          next()
        }
        
        id <- model$id
        # check if id already exist in components id
        if (any(grepl(pattern = paste0("^",id,"."), 
                      x = private$pComponentsId, perl = TRUE)) ||
            id %in% private$pComponentsId) { 
          sdCoupledModelMsg$addComponent1(private$pId, id)
          
          self$removeComponent(c(private$pComponentsId[
            grepl(pattern = paste0("^",id,"."), 
                  x = private$pComponentsId, perl = TRUE)], id))
        } else if (id == private$pId) {  
          # check if id is equal the coupled model id
          
          sdCoupledModelMsg$addComponent0(private$pId, id)
          next()
        }
        
        # only add sdOdeModel's or sdStaticModel's
        if (inherits(model, sdOdeModelClass$classname)) { 
          private$pComponentsId <- c(private$pComponentsId, id)
          private$pComponents[[id]] <- model$clone(deep = TRUE)
          private$pComponentsClass[[id]] <- class(model)[[1]]
          private$pComponentsEquations[[id]] <- model$ode
          private$pComponentsInitVars[[id]] <- model$initVars
          private$pComponentsPostProcess[[id]] <- model$postProcess
          
          if (is.function(model$trigger) || 
              is.numeric(model$trigger)) { 
            private$pComponentsTrigger[[id]] <- 
              model$trigger
            private$pComponentsEvent[[id]] <- model$event
          } else if (is.data.frame(model$trigger)) { 
            rootdf <- model$trigger
            rootdf$var <- paste0(id, ".", as.character(rootdf$var))
            # convert the columns for errors prevention
            rootdf$method <- lapply(as.character(rootdf$method), 
                                    type.convert, as.is = TRUE)
            rootdf$time <- as.numeric(rootdf$time)
            rootdf$value <- as.numeric(rootdf$value)
            
            private$pComponentsTrigger[[id]] <- rootdf
            private$pComponentsEvent[[id]] <- rootdf
          }
          
          private$pComponentsAux[[id]] <- model$aux
          private$pComponentsGlobal[[id]] <- model$globalFunctions
        } else if (inherits(model, sdStaticModelClass$classname)) { 
          private$pComponentsId <- c(private$pComponentsId, id)
          private$pComponents[[id]] <- model$clone(deep = TRUE)
          private$pComponentsClass[[id]] <- class(model)[[1]]
          private$pComponentsInitVars[[id]] <- model$initVars
          
          private$pComponentsAux[[id]] <- model$algebraicEquations
          private$pComponentsGlobal[[id]] <- model$globalFunctions
        } else if (inherits(model, sdCoupledModelClass$classname)) { 
          # add the components with the coupled model id as prefix
          for (j in seq_along(model$components)) { 
            comp <- model$components[[j]]$clone(deep = TRUE)
            comp$id <- paste0(model$id, ".", comp$id)
            
            self$addComponent(comp)
          }
          
          # add the connections
          for (con in model$connections) { 
            con[[2]] <- paste0(model$id, ".", con[[2]])
            con[[4]] <- paste0(model$id, ".", con[[4]])
            
            # private$pConnections[[con[[1]]]] <- con
            self$addConnection(con)
          }
        }
      }
      # merge the components auxs
      private$pComponentsAux <- lapply(unlist(private$pComponentsAux, 
                                              recursive = FALSE), 
                                       as.expression)
      private$flagBuild <- private$flagVerify <- FALSE
      invisible()
    },
    removeComponent = function(...) { 
      componentName <- gsub(" ", "", as.character(unlist(list(...))), 
                            fixed = TRUE)
      
      if (!all(componentName %in% private$pComponentsId)) { 
        warning(sprintf(sdCoupledModelMsg$removeComponent, private$pId, 
                        paste(componentName[!(componentName %in% 
                                                private$pComponentsId)])), 
                call. = FALSE)
        componentName <- componentName[componentName %in% private$pComponentsId]
      }
      
      # remove the models from all lists
      private$pComponentsId <- private$pComponentsId[
        !(private$pComponentsId %in% componentName)]
      
      private$pComponents <- private$pComponents[
        !(names(private$pComponents) %in% componentName)]
      
      private$pComponentsEquations <- private$pComponentsEquations[
        !(names(private$pComponentsEquations) %in% componentName)]
      
      private$pComponentsInitVars <- private$pComponentsInitVars[
        !(names(private$pComponentsInitVars) %in% componentName)]
      
      private$pComponentsPostProcess <- private$pComponentsPostProcess[
        !(names(private$pComponentsPostProcess) %in% componentName)]
      
      private$pComponentsTrigger <- 
        private$pComponentsTrigger[
          !(names(private$pComponentsTrigger) %in% componentName)]
      
      private$pComponentsEvent <- private$pComponentsEvent[
        !(names(private$pComponentsEvent) %in% componentName)]
      
      for (model in componentName)
        private$pComponentsAux <-  private$pComponentsAux[
          !grepl(paste0("^", model, "\\."), 
                 names(private$pComponentsAux))]
      
      private$pComponentsGlobal <- private$pComponentsGlobal[
        !(names(private$pComponentsGlobal) %in% componentName)]
      
      # remove the connections involving the removed model
      connections <- private$pConnections
      if (!is.null(connections) && length(connections) > 0) { 
        for (model in componentName) { 
          remCon <- sapply(1:length(connections), function(i) { 
            if (model %in% connections[[i]])
              return(connections[[i]][[1]])
          })
          
          if (length(remCon) > 0)
            self$removeConnection(remCon)
        }
      }
      private$flagBuild <- private$flagVerify <- FALSE
    },
    addConnection = function(...) { 
      connections <- list(...)
      
      for (con in connections) { 
        if (length(con) != 5) {
          warning(sprintf(sdCoupledModelMsg$addConnection1, private$pId, 
                          paste(con, collapse = ",")), call. = FALSE)
        } else if (!grepl(pattern = "^(aux\\$|st\\$|eq\\$)", x = con[[5]], 
                        perl = TRUE)) {
          warning(sprintf(sdCoupledModelMsg$addConnection2,private$pId,paste(con, collapse = ",")))
        } else { 
          # add connection
          if (con[[1]] %in% names(private$pConnections))
            sdCoupledModelMsg$addConnection3(private$pId, con[[1]])
          
          private$pConnections[[con[[1]]]] <- con
          
        }
      }
      
      private$flagBuild <- private$flagVerify <- FALSE
    },
    removeConnection = function(...) { # ... = cons ids
      connectionsId <- as.character(unlist(list(...)))
      auxComponents <- private$pComponentsAux
      
      # refactor the aux lists removing the connection dependency
      for (conId in connectionsId) { 
        con <- private$pConnections[[conId]]
        
        if (!is.null(con)) { 
          id <- con[[1]]
          m1 <- con[[2]]
          in1 <- con[[3]]
          m2 <- con[[4]]
          out2 <- strsplit(con[[5]], "$", fixed = TRUE)[[1]]
          
          # remove the aux connection from the equations list using regex
          if (out2[[1]] %in% c("aux", "eq")) { 
            auxComponents[grepl(paste0("^", m1, "\\."),
                                names(auxComponents))] <- 
              lapply(auxComponents[grepl(paste0("^", m1, "\\."),
                                         names(auxComponents))], function(x) { 
                                           gsub(pattern = paste0(out2[[1]], "\\$", m2, ".", out2[[2]]),
                                                replacement = paste0("inp$", m1, ".", in1),
                                                x = toString(as.expression(x)), perl = TRUE)
                                         })
            
            private$auxCon <- private$auxCon[names(private$auxCon) 
                                             !=  paste0(m1, ".", in1)]
          } else {
            private$stCon <- private$stCon[names(private$stCon) 
                                           !=  paste0(m1, ".", in1)]
          }
        } else { # invalid con
          warning(sprintf(sdCoupledModelMsg$removeConnection, private$pId, 
                          conId), call. = FALSE)
        }
      }
      # refactor the auxliary list without sorting
      private$pComponentsAux <- sdInitEquations(auxComponents, eqName = "NULL")
      
      # remove the connections with id in the connecitonId vector
      private$pConnections <- private$pConnections[!(names(private$pConnections) 
                                                     %in% connectionsId)]
      private$flagVerify <- private$flagBuild <- FALSE
    },
    verifyModel = function(scenario = NULL, verbose = FALSE,
                           timeSeriesDirectory = "") { 
      if (!self$isBuilt)
        stop(sprintf(sdCoupledModelMsg$verifyModel1, private$pId))
      
      # simulate the coupled model first time step
      # Get components functions
      componentsEquations <- private$pComponentsEquations
      namesCompEqs <- names(private$pComponentsEquations)
      componentsInitVars <- private$pComponentsInitVars
      componentsTrigger <- private$pComponentsTrigger
      componentsEvent <- private$pComponentsEvent
      eq <- aux <- private$pComponentsAux
      componentsId <- private$pComponentsId
      iComps <- private$pindexComponents
      
      # get the model scenario 
      defaultScenario <- self$defaultScenario$clone(deep = TRUE)
      
      # overwrite default variables with the given scenario values
      if (!is.null(scenario)) { 
        # convert list of scenarios to coupled scenario
        if (is.list(scenario))
          scenario <- sdBuildCoupledScenario(id = "coupledScen", 
                                             scenarios = scenario)
        else if (is.character(scenario))
          scenario <- sdLoadScenario(file = scenario)
        
        if (inherits(scenario, sdScenarioClass$classname))
          defaultScenario <- mergeScenarios(defaultScenario, scenario)
        else
          warning(sprintf(sdCoupledModelMsg$verifyModel2,private$pId,typeof(scenario)))
      }
      # get model variables
      st <- defaultScenario$state
      ct <- defaultScenario$constant
      par <- defaultScenario$parameter
      inp <- defaultScenario$input
      sw <- defaultScenario$switch
      times <- defaultScenario$times
      
      # Run the model Init Vars
      modelInit <- list()
      for (modelId in names(componentsInitVars)) { 
        # run the init vars
        if (!is.null(componentsInitVars[[modelId]])) { 
          if (inherits(private$pComponents[[modelId]], 
                       sdOdeModelClass$classname))
            modelInitVars <- componentsInitVars[[modelId]](
              st = st[iComps$st[[modelId]]],
              ct = ct[iComps$ct[[modelId]]],
              par = par[iComps$par[[modelId]]],
              inp = inp[iComps$inp[[modelId]]],
              sw = sw[iComps$sw[[modelId]]],
              aux = aux[iComps$aux[[modelId]]])
          else if (inherits(private$pComponents[[modelId]], 
                            sdStaticModelClass$classname))
            modelInitVars <- componentsInitVars[[modelId]](
              ct = ct[iComps$ct[[modelId]]],
              par = par[iComps$par[[modelId]]],
              inp = inp[iComps$inp[[modelId]]],
              sw = sw[iComps$sw[[modelId]]],
              eq = aux[iComps$aux[[modelId]]])
          
          #concatenate the initVars return
          modelInit$st <- c(modelInit$st, modelInitVars$st)
          modelInit$ct <- c(modelInit$ct, modelInitVars$ct)
          modelInit$par <- c(modelInit$par, modelInitVars$par)
          modelInit$inp <- c(modelInit$inp, modelInitVars$inp[
            !(names(modelInitVars$inp) %in% c("interpolation_", "fun_"))])
          modelInit$sw <- c(modelInit$sw, modelInitVars$sw)
        }
      }
      if (length(modelInit) > 0) { 
        st  <- MergeLists(modelInit$st, st, "coupledState")
        ct  <- MergeLists(modelInit$ct, ct, "coupledConstant")
        par <- MergeLists(modelInit$par, par, "coupledParameter")
        inp <- MergeLists(modelInit$inp, inp, "coupledInput")
        sw  <- MergeLists(modelInit$sw, sw, "coupledSwitch")
      }
      
      # get the connections list
      stCon <- private$stCon
      auxCon <- private$auxCon
      # get the aux connections index
      conAux <- match(unlist(auxCon, use.names = FALSE), names(aux))
      conAuxInps <- match(names(auxCon), names(inp))
      # get the connected st index and the connected inp index
      conSt <- match(unlist(stCon, use.names = FALSE), names(st))
      conStInps <- match(names(stCon), names(inp))
      
      if (!is.null(times) && length(unlist(times)) > 0) { 
        t <- times[[1]]
      } else { 
        warning(sprintf(sdCoupledModelMsg$verifyModel3, private$pId))
        t <- 0
      }
      
      # evaluate time series varibles 
      # compute the time series in the inputs
      for (var in names(inp$fun_)) { 
        if (!is.null(inp$fun_[[var]]))
          inp[[var]] <- inp$fun_[[var]](t)
        else
          inp[[var]] <- NULL
      }
      # make the connections using the transformation vector
      inp[conStInps] <- st[conSt]
      
      auxenv <- new.env()
      appendEnv(auxenv, private$pCoupledEnv)
      
      # evaluate the auxiliary variables and update the aux list
      for (auxVar in names(aux)) { 
        eq[[auxVar]] <- aux[[auxVar]] <- tryCatch( { 
          eval(aux[[auxVar]],
               envir = auxenv)
        },
        error = function(e) { 
          warning(sprintf(sdCoupledModelMsg$verifyModel4, private$pId,
                          auxVar, e))
          invisible(numeric(0))
        })
        
        if (is.null(aux[[auxVar]]) || is.na(aux[[auxVar]]) ||
            length(aux[[auxVar]]) == 0 || is.infinite(aux[[auxVar]]))
          sdCoupledModelMsg$verifyModel5(private$pId, aux, auxVar)
      }
      # make the aux connection
      inp[conAuxInps] <- aux[conAux]
      
      if (length(private$pComponentsEquations) > 0) { # at least one atomic model components
        # Create new environment to store variables that will be validated
        # and set the coupled env as its parent environment
        env <- new.env(parent = private$pCoupledEnv)
        lsVars <- list()
        
        # replace original function with test function
        for (i in 1:length(componentsEquations)) { 
          mId <- namesCompEqs[[i]]
          lsVars[[mId]] <- c()
          bodyStr <- as.character(body(componentsEquations[[i]]))
          bodyStr <- gsub("(?<!<)<-", "<<-", bodyStr, perl = TRUE)
          bodyStr <- gsub("->(?!>)", "->>", bodyStr, perl = TRUE)
          bodyStr <- paste(bodyStr[2:length(bodyStr)], collapse = "\n")
          body(componentsEquations[[i]]) <- 
            parse(text = paste("{", bodyStr, "}", sep = "\n"))
          lsVars[[mId]] <- c(lsVars[[mId]],
                             all.vars(body(componentsEquations[[i]])))
          environment(componentsEquations[[i]]) <- env
        }
        
        # Create test variables in env
        for (var in unlist(lsVars, use.names = FALSE))
          assign(var, "\\0", envir = env)
        
        # stores the model definitions result
        dState <- c()
        dAux <- list()
        
        # run the components model definitions
        dState <- vector("numeric", length = length(st))
        names(dState) <- names(st)
        for (i in seq_along(namesCompEqs)) { 
          # run the model definition
          mDef <- tryCatch( { 
            componentsEquations[[i]](t = t, 
                                     st = st[iComps$st[[namesCompEqs[[i]]]]], 
                                     ct = ct[iComps$ct[[namesCompEqs[[i]]]]], 
                                     par = par[iComps$par[[namesCompEqs[[i]]]]],
                                     inp = inp[iComps$inp[[namesCompEqs[[i]]]]], 
                                     sw = sw[iComps$sw[[namesCompEqs[[i]]]]], 
                                     aux = aux[iComps$aux[[namesCompEqs[[i]]]]])
          },
          error = function(e) { 
            warning(sprintf(sdCoupledModelMsg$verifyModel6, private$pId,
                            namesCompEqs[[i]], e))
            invisible(NULL)
          })
          
          if (length(unlist(mDef[[1]])) == 0)
            mDef[[1]] <- c()
          
          # concatenate the states derivatives
          dState[iComps$st[[namesCompEqs[[i]]]]] <- mDef[[1]]
          # concatenate the global auxiliary values
          mAux <- mDef[-1]
          
          if (length(mAux) > 0) { # there is auxiliary values
            names(mAux)[names(mAux) == ""] <- "noname"
            names(mAux) <- paste0(namesCompEqs[[i]], ".", names(mAux))
            dAux <- append(dAux, mAux)
          }
        }
        res <- list(c(dState), dAux, aux)
        
        # Display warnings if any variables during the Model Definition
        # execution are NULL, numeric(0), Inf or NA
        for (modelId in namesCompEqs) { 
          for (x in lsVars[[modelId]]) { 
            var <- mget(x, envir = environment(componentsEquations[[modelId]]),
                        ifnotfound = "\\1", inherits = TRUE)[[1]]
            
            if (is.function(var) || is.environment(var)) { 
              # do nothing
            } else if ( (is.list(var) && length(var) > 0) || 
                      ( is.vector(var) && length(var) > 1 )) { 
              
              xUnlist <- unlist(var, recursive = TRUE)
              
              for (i in 1:length(xUnlist)) { 
                if (is.function(xUnlist[[i]]) || is.environment(xUnlist[[i]]) ||
                    is.language(xUnlist[[i]])) {
                  next # do nothing
                } else if (is.null(xUnlist[[i]])) {
                  warning(sprintf(sdCoupledModelMsg$verifyModel7, private$pId,
                                  modelId, names(xUnlist)[[i]], x, "NULL"))
                } else if (length(xUnlist[[i]]) == 0 && is.numeric(xUnlist[[i]])) {
                  warning(sprintf(sdCoupledModelMsg$verifyModel7, private$pId,
                                  modelId, names(xUnlist)[[i]], x, "numeric(0)"))
                } else if (is.na(xUnlist[[i]])) {
                  warning(sprintf(sdCoupledModelMsg$verifyModel7, private$pId,
                                  modelId, names(xUnlist)[[i]], x, "NA"))
                } else if (is.infinite(xUnlist[[i]])) {
                  warning(sprintf(sdCoupledModelMsg$verifyModel7, private$pId,
                                  modelId, names(xUnlist)[[i]], x, "Inf"))
                }
              } 
            }
            else if (x %in% c('st', 'ct', 'par', 'inp', 'sw', 'aux'))
              next # do nothing if an arg is empty
            else if (is.null(unlist(var)))
              warning(sprintf(sdCoupledModelMsg$verifyModel8, private$pId,
                              modelId, x, "NULL"))
            else if (length(var) == 0 && is.numeric(var))
              warning(sprintf(sdCoupledModelMsg$verifyModel8, private$pId,
                              modelId, x, "numeric(0)"))
            else if (is.na(unlist(var)))
              warning(sprintf(sdCoupledModelMsg$verifyModel8, private$pId,
                              modelId, x, "NA"))
            else if (is.infinite(unlist(var)))
              warning(sprintf(sdCoupledModelMsg$verifyModel8, private$pId,
                              modelId, x, "Inf"))
          }
        }
        
        # Check the return of Model Definition contains invalid values
        if (is.list(res)) { 
          dRes <- res[[1]]
          
          if (!is.numeric(dRes))
            warning(sprintf(sdCoupledModelMsg$verifyModel9, private$pId, 
                            typeof(dRes)))
          
          if (length(dRes) != length(st))
            sdCoupledModelMsg$verifyModel10(private$pId, dRes,
                                            length(st))
        } else {
          warning(sprintf(sdCoupledModelMsg$verifyModel11, private$pId,
                          typeof(res)))
        }
      }
      
      if (verbose)
        sdCoupledModelMsg$verifyModel12(private$pId)
      
      private$flagVerify <- TRUE
    },
    
    buildCoupledModel = function(from = NULL,
                                 to = NULL,
                                 by = NULL,
                                 method = c("lsoda", "lsode", 
                                            "lsodes", "lsodar", 
                                            "vode", "daspk",
                                            "euler", "rk4", "ode23", 
                                            "ode45", "radau", 
                                            "bdf", "bdf_d", "adams", 
                                            "impAdams", "impAdams_d"),
                                 timeSeriesDirectory = "") { 
      if (length(private$pComponentsId) == 0)
        stop(sprintf(sdCoupledModelMsg$buildCoupledModel1, private$pId), 
             call. = FALSE)
      
      # build default scenario concatanating the components default scenario
      componentsIds <- private$pComponentsId
      scenComponents <- list()
      auxsNames <- list()
      
      for (modelId in componentsIds) { 
        if (!is.null(private$pComponents[[modelId]]$defaultScenario))
          scenComponents[[modelId]] <-
            private$pComponents[[modelId]]$defaultScenario$clone()
      }
      
      defaultScenarioVars <- sdBuildCoupledScenario(
        id = "Default",
        scenarios = scenComponents,
        method = method[[1]],
        from = from,
        to = to,
        by = by,
        varNames = TRUE,
        timeSeriesDirectory = timeSeriesDirectory)
      private$pDefaultScenario <-
        defaultScenarioVars$coupledScenario
      
      # build the components list of indexes
      indexComponents <- list(st = list(), ct = list(), inp = list(), 
                              par = list(), ts = list(), sw = list(), 
                              aux = list())
      namesSt <- names(defaultScenarioVars$coupledScenario$state)
      namesCt <- names(defaultScenarioVars$coupledScenario$constant)
      namesInp <- names(defaultScenarioVars$coupledScenario$input)
      namesTs <- names(defaultScenarioVars$coupledScenario$input$fun_)
      namesPar <- names(defaultScenarioVars$coupledScenario$parameter)
      namesSw <- names(defaultScenarioVars$coupledScenario$switch)
      
      coupledEnv <- private$pCoupledEnv
      # clean coupledEnv
      rm(list = ls(coupledEnv), envir = coupledEnv)
      
      # get all the components variables names and substitute in the components
      # functions for the coupled names (concatenated with the model ID)
      # reset the enviromnet of all components functions with the coupled env
      componentsVarNames <- defaultScenarioVars$varNames
      for (modelId in componentsIds) { 
        componentsVarNames[[modelId]] <- c(
          componentsVarNames[[modelId]],
          gsub(pattern = paste0("^",modelId,"\\."), replacement = "", 
               x = names(private$pComponentsAux)[
                 grepl(paste0("^", modelId, "\\."),
                       names(private$pComponentsAux))]),
          names(private$pComponentsGlobal[[modelId]]))
        
        if (is.function(private$pComponentsEquations[[modelId]])) { 
          private$pComponentsEquations[[modelId]] <-
            replaceCoupledVarsNames(
              func = private$pComponentsEquations[[modelId]],
              componentsVarNames = componentsVarNames[[modelId]],
              modelId = modelId)
          
          environment(private$pComponentsEquations[[modelId]]) <-
            coupledEnv
        }
        
        if (is.function(private$pComponentsInitVars[[modelId]])) { 
          private$pComponentsInitVars[[modelId]] <-
            replaceCoupledVarsNames(
              func = private$pComponentsInitVars[[modelId]],
              componentsVarNames = componentsVarNames[[modelId]],
              modelId = modelId)
          
          environment(private$pComponentsInitVars[[modelId]]) <-
            coupledEnv
        }
        
        if (is.function(private$pComponentsPostProcess[[modelId]])) { 
          private$pComponentsPostProcess[[modelId]] <-
            replaceCoupledVarsNames(
              func = private$pComponentsPostProcess[[modelId]],
              componentsVarNames = componentsVarNames[[modelId]],
              modelId = modelId)
          
          environment(private$pComponentsPostProcess[[modelId]]) <-
            coupledEnv
        }
        
        if (is.function(private$pComponentsTrigger[[modelId]])) { 
          private$pComponentsTrigger[[modelId]] <-
            replaceCoupledVarsNames(
              func = private$pComponentsTrigger[[modelId]],
              componentsVarNames = componentsVarNames[[modelId]],
              modelId = modelId)
          
          environment(private$pComponentsTrigger[[modelId]]) <-
            coupledEnv
        }
        
        if (is.function(private$pComponentsEvent[[modelId]])) { 
          private$pComponentsEvent[[modelId]] <-
            replaceCoupledVarsNames(
              func = private$pComponentsEvent[[modelId]],
              componentsVarNames = componentsVarNames[[modelId]],
              modelId = modelId)
          
          environment(private$pComponentsEvent[[modelId]]) <-
            coupledEnv
        }
        
        private$pComponentsAux[grepl(paste0("^", modelId, "\\."),
                                     names(private$pComponentsAux))] <-
          replaceCoupledVarsNames(
            listExp = private$pComponentsAux[
              grepl(paste0("^", modelId, "\\."),
                    names(private$pComponentsAux))],
            componentsVarNames = componentsVarNames[[modelId]],
            modelId = modelId)
        
        if (length(private$pComponentsGlobal[[modelId]]) > 0) { 
          for (i in 1:length(private$pComponentsGlobal[[modelId]])) { 
            if (is.function(private$pComponentsGlobal[[modelId]][[i]])) { 
              
              private$pComponentsGlobal[[modelId]][[i]] <-
                replaceCoupledVarsNames(
                  func = private$pComponentsGlobal[[modelId]][[i]],
                  componentsVarNames = componentsVarNames[[modelId]],
                  modelId = modelId)
              
              environment(private$pComponentsGlobal[[modelId]][[i]]) <-
                coupledEnv
              
            } else if (is.expression(
              private$pComponentsGlobal[[modelId]][[i]])) {
              
              private$pComponentsGlobal[[modelId]][[i]] <-
                replaceCoupledVarsNames(
                  listExp = private$pComponentsGlobal[[modelId]][[i]],
                  componentsVarNames = componentsVarNames[[modelId]],
                  modelId = modelId)
            }
          }
        }
        
        # stop if components are empty
        if (length(private$pComponentsEquations) == 0 && 
            length(private$pComponentsAux) == 0)
          stop(sprintf(sdCoupledModelMsg$buildCoupledModel0, private$pId), 
               call. = FALSE) 
        
        # set each component variables indexes
        indexComponents$st[[modelId]] <- match(
          namesSt[grepl(paste0("^", modelId, "\\."), namesSt)], namesSt)
        indexComponents$ct[[modelId]] <- match(
          namesCt[grepl(paste0("^", modelId, "\\."), namesCt)], namesCt)
        indexComponents$inp[[modelId]] <- match(
          namesInp[grepl(paste0("^", modelId, "\\."), namesInp)], namesInp)
        indexComponents$ts[[modelId]] <- match(
          namesTs[grepl(paste0("^", modelId, "\\."), namesTs)], namesTs)
        indexComponents$par[[modelId]] <- match(
          namesPar[grepl(paste0("^", modelId, "\\."), namesPar)], namesPar)
        indexComponents$sw[[modelId]] <- match(
          namesSw[grepl(paste0("^", modelId, "\\."), namesSw)], namesSw)
      }
      
      # add the global function to the coupled env
      appendEnv(coupledEnv, 
                as.list(unlist(private$pComponentsGlobal, 
                               recursive = FALSE)))
      
      stComponents <- defaultScenarioVars$coupledScenario$state
      inpComponents <- defaultScenarioVars$coupledScenario$input
      auxComponents <- private$pComponentsAux
      
      # build the auxliary eq and state vars connections list
      eqConnections <- list()
      stConnections <- list()
      
      # Build the transformation vectors from the given connections
      connections <- private$pConnections
      if (!is.null(connections) && length(connections) > 0) { 
        
        for (con in connections) { 
          id <- con[[1]]
          m1 <- con[[2]]
          in1 <- con[[3]]
          m2 <- con[[4]]
          out2 <- strsplit(con[[5]], "$", fixed = TRUE)[[1]]
          
          if (!(m1 %in% componentsIds)) {
            
            warning(sprintf(sdCoupledModelMsg$buildCoupledModel2, private$pId,
                            m1, id))
            
          } else if (!(m2 %in% componentsIds)) {
            
            warning(sprintf(sdCoupledModelMsg$buildCoupledModel2, private$pId,
                            m2, id))
            
          } else if (!(paste0(m1, ".", in1) %in% names(inpComponents))) {
            
            warning(sprintf(sdCoupledModelMsg$buildCoupledModel3,private$pId,
                            "input", in1, m1, id))
            
          } else if (!(out2[[1]] %in% c("aux", "st", "eq"))) {
            
            warning(sprintf(sdCoupledModelMsg$buildCoupledModel4,
                            private$pId, m2, id))
            
          } else if (out2[[1]] == "st" && !(paste0(m2, ".", out2[[2]]) %in% 
                                          names(stComponents))) {
            
            warning(sprintf(sdCoupledModelMsg$buildCoupledModel3,private$pId,
                            "state", out2[[2]], m2, id))
            
          } else if (out2[[1]] %in% c("aux", "eq") && 
                   !(paste0(m2, ".", out2[[2]]) %in% names(auxComponents))) {
            
            warning(sprintf(sdCoupledModelMsg$buildCoupledModel3,private$pId,
                            "equation", out2[[2]], m2, id))
            
          } else { 
            # check connections unit
            u1 <- defaultScenarioVars$coupledScenario$unit[[
              paste0(m1, ".", in1)]]
            u2 <- defaultScenarioVars$coupledScenario$unit[[
              paste0(m2, ".", out2[[2]])]]
            
            # warning if units do not match
            if (!is.null(u1) &&
                !is.null(u2) && toupper(u1) != toupper(u2))
              sdCoupledModelMsg$buildCoupledModel5(private$pId, in1, 
                                                   u1, m1, out2, u2, m2, id)
            # make connection
            if (out2[[1]] == "st") { 
              if (!is.null(stConnections[[paste0(m1, ".", in1)]]))
                warning(sprintf(sdCoupledModelMsg$buildCoupledModel6,
                                private$pId,paste0(m1, ".", in1),
                                "state variable"))
              
              stConnections[paste0(m1, ".", in1)] <- paste0(m2,".", out2[[2]])
            } else if (out2[[1]] %in% c("aux","eq")) { 
              if (!is.null(eqConnections[[paste0(m1, ".", in1)]]))
                warning(sprintf(sdCoupledModelMsg$buildCoupledModel6,
                                private$pId,paste0(m1, ".", in1),
                                "equation"))
              
              eqConnections[paste0(m1, ".", in1)] <- paste0(m2, ".", 
                                                            out2[[2]])
              # substitute the connected inp <- aux
              auxComponents <- lapply(auxComponents, function(x) { 
                gsub(pattern = paste0("inp\\$", m1, ".", in1, 
                                      "|inp\\[(\\'|\\\")", m1, ".", in1,
                                      "\b(\\'|\\\")\\]|inp\\[\\[(\\'|\\\")",
                                      m1, ".", in1, "\b(\\'|\\\")\\]\\]"),
                     replacement = paste0(out2[[1]], "$", 
                                          m2, ".", out2[[2]]),
                     x = toString(as.expression(x)), perl = TRUE)
              })
            }
          }
        }
      }
      # sort the auxliary equations and the connection matrix
      auxComponents <- sdInitEquations(auxComponents, eqName = c("aux", "eq"))
      private$pComponentsAux <- auxComponents
      
      # set index list for the equations
      namesAux <- names(auxComponents)
      for (modelId in componentsIds)
        indexComponents$aux[[modelId]] <- match(
          namesAux[grepl(paste0("^", modelId, "\\."), namesAux)], namesAux)
      # set the index list
      private$pindexComponents <- indexComponents
      
      private$auxCon <- eqConnections
      private$stCon <- stConnections
      
      private$flagBuild <- TRUE
    },
    saveXml = function(file = "sdCoupledModel.xml") { 
      # save model to XML
      doc = XML::newXMLDoc()
      rootsdCoupledModel <- XML::newXMLNode(class(self)[[1]], doc = doc)
      
      lCoupledModel <-
        list(
          id = private$pId,
          description = private$pDescription,
          connections = private$pConnections
        )
      ListToXML(rootsdCoupledModel, lCoupledModel)
      
      # add the components
      components <- XML::newXMLNode("components")
      componentsXML <- lapply(private$pComponents, function(x) { 
        x$saveXml()
      })
      XML::addChildren(components, kids = componentsXML)
      XML::addChildren(rootsdCoupledModel, kids = list(components))
      
      if (!missing(file))
        cat(XML::saveXML(doc, encoding = "UTF-8", 
                         prefix = xmlPrefix(),
                         indent = TRUE),  file = file) 
      
      invisible(rootsdCoupledModel)
    }
  ),
  active = list(
    defaultScenario = function() { 
      if (private$flagBuild)
        return(private$pDefaultScenario)
      else
        warning(sprintf(sdCoupledModelMsg$defaultScenario,private$pId))
      
      invisible(NULL)
    },
    components = function() { 
      return(private$pComponents)
    },
    componentsId = function() { 
      return(private$pComponentsId)
    },
    componentsClass = function() { 
      return(private$pComponentsClass)
    },
    componentsEquations = function() { 
      return(private$pComponentsEquations)
    },
    componentsInitVars = function() { 
      return(private$pComponentsInitVars)
    },
    componentsPostProcessVars = function() { 
      return(private$pComponentsPostProcess)
    },
    componentsTrigger = function() { 
      return(private$pComponentsTrigger)
    },
    componentsEvent = function() { 
      return(private$pComponentsEvent)
    },
    componentsAux = function() { 
      return(private$pComponentsAux)
    },
    componentsGlobal = function() { 
      return(private$pComponentsGlobal)
    },
    connections = function() { 
      return(private$pConnections)
    },
    eqConnections = function() { 
      # check if the matrix exist - flagBuild = TRUE
      if (private$flagBuild)
        return(private$auxCon)
      else
        warning(sprintf(sdCoupledModelMsg$connectionsList, 
                        private$pId,"equations"))
      invisible(NULL)
    },
    stConnections = function() { 
      # check if the matrix exist - flagBuild = TRUE
      if (private$flagBuild)
        return(private$stCon)
      else
        warning(sprintf(sdCoupledModelMsg$connectionsList, 
                        private$pId,"state"))
      invisible(NULL)
    },
    indexComponents = function() { 
      # check if the matrix exist - flagBuild = TRUE
      if (private$flagBuild)
        return(private$pindexComponents)
      else
        warning(sprintf(sdCoupledModelMsg$indexComponents,private$pId))
      
      invisible(NULL)
    },
    isBuilt = function() { 
      return(private$flagBuild)
    },
    modelEnv = function() { 
      return(private$pCoupledEnv)
    }
  ),
  private = list(
    # Class Private Atributes
    pindexComponents = NULL,
    pConnections = list(),
    auxCon = NULL,
    stCon = NULL,
    flagBuild = FALSE,
    flagVerify = FALSE,
    pComponents = list(),
    pComponentsId = list(),
    pComponentsClass = list(),
    pComponentsEquations = list(),
    pComponentsInitVars = list(),
    pComponentsPostProcess = list(),
    pComponentsTrigger = list(),
    pComponentsEvent = list(),
    pComponentsAux = list(),
    pComponentsGlobal = list(),
    pCoupledEnv = NULL
  )
)
