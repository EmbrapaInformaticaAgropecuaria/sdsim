#' Creates an Atomic System Dynamics Model Object
#' 
#' A factory function that creates a \code{\link{sdOdeModelClass}} object from 
#' functions that describe the changes in the system states in response to 
#' external actions and other system flows, and a default scenario describing 
#' the system environment (variables and default values). 
#' 
#' To load a model from a XML file use the \code{\link{sdLoadModel}} function.
#' 
#' To simulate a model in different scenarios use the \code{\link{sdSimulate}}
#' function.
#' 
#' @param id A character string with the model ID. It is coerced to a valid id 
#' following the rules described in \code{\link{sdsim-LabelingRules}}.
#' @param description A character string with the model description.
#' @param defaultScenario The model default scenario, a 
#' \code{\link{sdScenarioClass}} object. It should contain all the model
#' variables initialized with default values that ensures the model simulation.
#' @param aux (Optional) A list with the model auxiliary equations in strings or 
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
#' @param ode An R-function that computes the values of the 
#' state variables derivatives in the ODE system (the model definition) at time 
#' t. 
#' 
#' It must be defined as: function(t, st, ct, par, inp, sw, aux). 
#' Where \code{t} is the current time point in the integration, \code{st} is 
#' a list with the current estimate of the state variables in the ODE system, 
#' \code{ct} is a list with the model constant variables, \code{par} is
#' a list with the model parameter variables, \code{inp} is a list with the
#' model input variables (which will have the time series variables evaluated 
#' for each time step during the simulation), \code{sw} is list with the model 
#' switch variables and \code{aux} is a list with the model auxiliary equations 
#' (which will be evaluated for each time step during the simulations).
#' 
#' The return value of the \code{ode} must be a list, whose 
#' first element is a vector containing the derivatives of the state variables 
#' with respect to time, and whose next elements are extra values that are 
#' computed at each time step and need to be included in the simulation output. 
#' The derivatives must be specified in the same order as the state variables.
#' @param initVars (Optional) An R-function that initialize or change the 
#' initial state values and/or other model variables before the solver call when
#' running a simulation. 
#' It can be used, for example, to compute some dependent parameter variables or 
#' the initial state variables, using the arguments. 
#' 
#' It must be defined as function(st, ct, par, inp, sw, aux). Where \code{st} is 
#' a list with the initial state variables values, \code{ct} is a list with the 
#' model constant variables, \code{par} is a list with the model parameter
#' variables, \code{inp} is a list with the model input variables, \code{sw} is
#' a list with the model switch variables and \code{aux} is a list with the 
#' model auxiliary equations in R-expression format, as defined by the user.
#' 
#' The return value of the \code{initVars} function must be a list containing 
#' all the variable arguments named in the same way, except the aux equations; 
#' e.g. \code{return(list(st = st, ct = ct, inp = inp, par = par, sw = sw))}.
#' @param postProcess (Optional) An R-function that receives the simulation 
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
#' @param trigger (Optional) A numeric vector containing the times to 
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
#' @param event (Optional) An R-function that specifies the event. 
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
#' @param globalFunctions A named list of extra functions that can be executed 
#' in the scope of any other function or auxiliary equation defined in the 
#' model. They can be called using the list names.
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
#' lv <- sdOdeModel(id = "Lotka-Volterra",
#'                    defaultScenario = lvscen,
#'                    ode = LVode,
#'                    aux = aux)
#'               
#' # validate the model ode
#' lv$verifyModel(verbose = TRUE)
#' 
#' # simulate the model and plot the results
#' outlv <- sdSimulate(model = lv, storeAuxTrajectory = TRUE)
#' outlv$plot("P C", multipleYAxis = TRUE, 
#'            main = "Prey and Consumer by Lotka-Volterra")
#' outlv$saveSimulationOutput(path = "LV") 
#' @return A \code{\link{sdOdeModelClass}} object.
sdOdeModel <- function(id = NULL,
                       description = NULL,
                       defaultScenario = NULL,
                       aux = NULL,
                       ode = NULL, 
                       initVars = NULL,
                       postProcess = NULL, 
                       trigger = NULL, 
                       event = NULL,
                       globalFunctions = NULL) { 
  # create a new model
  model <- sdOdeModelClass$new(
    id = id,
    description = description,
    ode = ode,
    defaultScenario = defaultScenario,
    initVars = initVars,
    postProcess = postProcess, 
    trigger = trigger, 
    event = event,
    aux = aux,
    globalFunctions = globalFunctions)
  
  return(model)
}

#' Loads a Model from a XML File
#' 
#' A factory function that creates \code{\link{sdOdeModelClass}}, 
#' \code{\link{sdCoupledModelClass}} or \code{\link{sdStaticModelClass}} object 
#' from a XML file or from the package 
#' repository (see the available models in the \code{\link{sdRepository}} 
#' documentation).
#' 
#' The XML file must be created with the models object method '$saveXml' to 
#' garantee that it will be correctly loaded and validated (by checking the 
#' sdsim prefix).
#' 
#' @param file The XML file name with the model specification. If
#' repository = TRUE, \code{file} is the name of a model from the sdsim 
#' repository.
#' @param repository logical. If TRUE will use the \code{file} name to load a 
#' model from the sdsim package repository. See \code{\link{sdRepository}} for the
#' complete list of valid names for loading existing models. Default is FALSE.
#' @param timeSeriesDirectory The directory where time series inputs are stored 
#' (when passing the time series inputs via external files).
#' @return A \code{\link{sdOdeModelClass}}, a \code{\link{sdCoupledModelClass}} or 
#' a \code{\link{sdStaticModelClass}} object.
#' @examples 
#' # Load the Bouncing Ball model from the sdsim repository
#' bb <- sdLoadModel(file = "BouncingBall", repository = TRUE)
#' 
#' # simulate the model and plot the results
#' outbb <- sdSimulate(model = bb)
#' outbb$plot("height speed", multipleYAxis = TRUE, units = TRUE)
#' 
#' # save the model to a XML file and reload it
#' bb$saveXml(file = "bb.xml")
#' bb <- sdLoadModel(file = "bb.xml") 
sdLoadModel <- function(file, repository = F,
                        timeSeriesDirectory = "") { 
  if (missing(file))
    stop(sprintf(constructorsMsg$sdLoadModel1,
                 paste(sdRepository(), collapse = "\n  - ")))
  
  if (repository) { 
    filepath <- system.file(appDir = paste0("repository/", file, ".xml"), 
                            package = "sdsim")
    if (filepath == "")
      stop(sprintf(constructorsMsg$sdLoadModel2,file,
                   paste(sdRepository(), collapse = "\n  - ")))
    else
      file <- filepath
  }
  
  # check if file exists
  if (!file.exists(file)) 
    stop(sprintf(constructorsMsg$sdLoadModel3,file))
  
  # read the file
  model <- tryCatch(
    XML::xmlParse(file),
    error = function(e) { 
      stop(sprintf(constructorsMsg$sdLoadModel4,file,e))
    },
    warning = function(w) { 
      warning(sprintf(constructorsMsg$sdLoadModel5, file, w))
    })
  
  # Check if file is a valid xml file from the sdsim package
  # check the file prefix
  sdsimprefix <- paste(readLines(file, n = 3), collapse = "\n")
  if (!grepl(pattern = "<\\?sdsim.*version.*\\?>", sdsimprefix, 
             ignore.case = TRUE))
    stop(sprintf(constructorsMsg$sdLoadModel6))
  
  # else
  # {
  #   # valid prefix, now check version
  #   if (!grepl(pattern = paste0("(?<=version=\\')", 
  #                               sub("\\.","\\\\.", packageVersion("sdsim"))), 
  #              x = sdsimprefix, ignore.case = TRUE, perl = TRUE))
  #     warning("Load Model: The sdsim XML version is deprecated. The current ",
  #             "sdsim version is: ", packageVersion("sdsim"))
  # }
  
  modelTags <- names(XML::xmlChildren(model))
  # convert the xml to list
  model <- XML::xmlToList(model)
  
  # instanciate the model object
  if (sdOdeModelClass$classname %in% modelTags) { # load atomic model
    # create the default scenario
    # convert the types of the xml vars
    if (is.list(model$defaultScenario)) { 
      loadedScen <- model$defaultScenario$sdScenario
      
      model$defaultScenario <- sdScenario(
        id = "Default",
        times = loadedScen$times,
        method = loadedScen$method,
        state = loadedScen$state,
        constant = loadedScen$constant,
        input = loadedScen$input,
        interpolation = loadedScen$interpolation,
        parameter = loadedScen$parameter,
        switch = loadedScen$switch,
        unit = loadedScen$unit,
        description = loadedScen$description,
        timeSeriesDirectory = timeSeriesDirectory)
    }
    
    # creat a new model
    model <- sdOdeModelClass$new(
      id = model$id,
      ode = StringToFun(model$ode),
      defaultScenario = model$defaultScenario,
      initVars = StringToFun(model$initVars),
      postProcess = StringToFun(model$postProcess),
      trigger = model$trigger,
      event = StringToFun(model$event),
      aux = model$aux ,
      description = model$description,
      globalFunctions = lapply(model$globalFunctions, 
                               StringToFun))
    
    return(model)
  } else if (sdCoupledModelClass$classname %in% modelTags) { # load coupled model
    if (!is.null(model$components)) { 
      # get the components Ids
      componentsId <- lapply(model$components, function(x) x$id)
      
      # convert the components
      model$components <- lapply(seq_along(model$components), function(i) { 
        # convert each component to a model object
        x <- model$components[[i]]
        # create the default scenario
        loadedScen <- x$defaultScenario$sdScenario
        
        x$defaultScenario <- 
          sdScenario(id = "Default",
                     times = loadedScen$times,
                     method = loadedScen$method,
                     state = loadedScen$state,
                     constant = loadedScen$constant,
                     input = loadedScen$input,
                     interpolation = loadedScen$interpolation,
                     parameter = loadedScen$parameter,
                     switch = loadedScen$switch,
                     unit = loadedScen$unit,
                     description = loadedScen$description,
                     timeSeriesDirectory = timeSeriesDirectory)
        
        if (names(model$components)[[i]] == sdOdeModelClass$classname) {
          # create a sd atomic model
          
          component <- sdOdeModelClass$new(
            id = x$id,
            description = x$description,
            ode = StringToFun(x$ode),
            defaultScenario = x$defaultScenario,
            initVars = StringToFun(x$initVars),
            postProcess = StringToFun(x$postProcess),
            trigger = x$trigger,
            event = StringToFun(x$event),
            aux = x$aux,
            globalFunctions = lapply(x$globalFunctions, StringToFun))
        } else if (names(model$components)[[i]] == sdStaticModelClass$classname) {
          # create a static model
          
          component <- sdStaticModelClass$new(
            id = x$id,
            description = x$description,
            initVars = StringToFun(x$initVars),
            algebraicEquations = x$algebraicEquations,
            defaultScenario = x$defaultScenario,
            globalFunctions = lapply(x$globalFunctions, StringToFun))
        } else {
          component <- NULL
        }
        
        component
      })
    }
    
    # convert the connections (parse text)
    model$connections <- lapply(model$connections, function(x) { 
      if (is.character(x) && length(x) == 1)
        eval(parse(text = x))
      else
        x
    })
    
    # creat a new coupled model
    coupledModel <- sdCoupledModelClass$new(
      id = model$id,
      description = model$description,
      components = model$components,
      connections = model$connections)
    
    
    return(coupledModel)
  } else if (sdStaticModelClass$classname %in% modelTags) { 
    # load static model
    
    # create the default scenario
    # convert the types of the xml vars
    if (is.list(model$defaultScenario)) { 
      loadedScen <- model$defaultScenario$sdScenario
      
      model$defaultScenario <- sdScenario(
        id = "Default",
        times = loadedScen$times,
        method = loadedScen$method,
        state = NULL,
        constant = loadedScen$constant,
        input = loadedScen$input,
        interpolation = loadedScen$interpolation,
        parameter = loadedScen$parameter,
        switch = loadedScen$switch,
        unit = loadedScen$unit,
        description = loadedScen$description,
        timeSeriesDirectory = timeSeriesDirectory)
    }
    
    # creat a new model
    model <- sdStaticModelClass$new(id = model$id,
                                    defaultScenario = model$defaultScenario,
                                    initVars = StringToFun(model$initVars),
                                    algebraicEquations = model$algebraicEquations,
                                    description = 
                                      model$description,
                                    globalFunctions = lapply(model$globalFunctions, 
                                                             StringToFun))
    
    return(model)
  } else {
    stop(sprintf(constructorsMsg$sdLoadModel7))
    
  }
}

#' Creates a Coupled System Dynamics Model
#' 
#' A factory function that creates a \code{\link{sdCoupledModelClass}} object 
#' made up of instanced
#' \code{\link{sdOdeModelClass}}, \code{\link{sdStaticModelClass}} and/or 
#' \code{\link{sdCoupledModelClass}} components and a list of connections that 
#' define the flow of information between components.
#'  
#' To load a coupled model from a XML file use the 
#' \code{\link{sdLoadModel}} function.
#' 
#' To simulate a coupled model in different scenarios use the 
#' \code{\link{sdSimulate}} function.
#' 
#' To build the default coupled scenario use the 
#' \code{\link{sdBuildCoupledScenario}} function. Make sure to build it before 
#' running the simulation to save some computation time.
#' 
#' @param id A character string with the coupled model ID. It is coerced to a 
#' valid id following the rules described in 
#' \code{\link{sdsim-LabelingRules}}.
#' @param description A character string with the coupled model 
#' description.
#' @param components A list of \code{\link{sdOdeModelClass}}, 
#' \code{\link{sdStaticModelClass}} and/or \code{\link{sdCoupledModelClass}} 
#' objects. 
#' 
#' The models must have different \code{ID}'s that will be used as unique 
#' identifiers, otherwise only the last model added with the same ID will be 
#' kept. 
#' @param connections A list of vectors that specifies the connections. 
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
#' @return A \code{\link{sdCoupledModelClass}} object.
#' @examples 
#' # The Lotka-Volterra consumer-prey model implemented as a coupled model with
#' # the components being the population models of a prey and a consumer
#' 
#' # set the time sequence and use the default method
#' times <- list(from = 0, to = 200, by = 1)
#' 
#' # Prey model variables and ode function
#' stPrey <- list(P = 1)
#' parsPrey      <- list(rG = 1.0,      
#'                       K  = 10)
#' inpPrey <- list(IngestC = 0)
#' auxPrey <- list(GrowthP = "par$rG * st$P * (1 - st$P/par$K)")
#' 
#' LVodePrey <- function(t, st, ct, par, inp, sw, aux) 
#' {
#'   dP    <- aux$GrowthP - inp$IngestC
#'   
#'   return(list(c(dP)))
#' }
#' 
#' # create the component prey model
#' prey <- sdOdeModel(
#'   id = "Prey",
#'   defaultScenario = sdScenario(id = "preyScen",
#'                                times = times,
#'                                state = stPrey,
#'                                parameter = parsPrey,
#'                                input = inpPrey),
#'   aux = auxPrey,
#'   ode = LVodePrey)
#' 
#' # Consumer model variables and ode function
#' stConsumer <- list(C = 2)
#' parsConsumer  <- list(rI = 0.2,
#'                       rM = 0.2 ,   
#'                       AE = 0.5) 
#' inpConsumer <- list(P = 0)
#' auxConsumer <- list(IngestC = "par$rI * inp$P * st$C",
#'                     "MortC <- par$rM * st$C")
#' 
#' LVodeConsumer <- function(t, st, ct, par, inp, sw, aux) 
#' {
#'   dC    <- aux$IngestC * par$AE - aux$MortC
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
#'     input = inpConsumer),
#'   aux = auxConsumer,
#'   ode = LVodeConsumer)
#' 
#' # create the coupled model connections list 
#' # conP: inform the consumer model about the amount of preys and 
#' # conIngestC: inform the prey model about the consumer ingestion
#' lvConnections <- list(c("conP", "Consumer", "P", "Prey", "st$P"),
#'                       c("conIngestC", "Prey", "IngestC", "Consumer", "aux$IngestC"))
#' 
#' # Create the Lotka-Volterra coupled model
#' coupledLV <- sdCoupledModel(id = "LVCoupled",
#'                             components = c(prey, consumer),
#'                             connections = lvConnections,
#'                             "Lotka-Volterra Equations implemented as coupled model")
#' # build the coupled model and validate it
#' coupledLV$buildCoupledModel(from = 0, 
#'                             to = 200, 
#'                             by = 1)
#' coupledLV$verifyModel(verbose = TRUE)
#' 
#' # simulate the coupled model and plot the results
#' outclv <- sdSimulate(model = coupledLV)
#' outclv$plot("Prey.P Consumer.C", 
#'             main = "Coupled Prey and Consumer by Lotka-Volterra",
#'             multipleYAxis = TRUE)
sdCoupledModel <- function(id = NULL,
                           description = NULL,
                           components = NULL,
                           connections = NULL) { 
  # creat a new model
  coupledModel <- sdCoupledModelClass$new(id = id,
                                          description = description,
                                          components = components,
                                          connections = connections)
  
  return(coupledModel)
}


#' Creates Static Model Object
#' 
#' A factory function that creates a \code{\link{sdStaticModelClass}} object 
#' from a list of algebraic
#' equations that calculates the system in equilibrium and a default scenario 
#' describing the system environment (variables and default values). 
#' 
#' To load a model from a XML file use the \code{\link{sdLoadModel}} function.
#' 
#' To simulate a model in different scenarios use the \code{\link{sdSimulate}}
#' function.
#' 
#' @param id A character string with the model ID. It is coerced to a valid id 
#' following the rules described in \code{\link{sdsim-LabelingRules}}.
#' @param description A string with the model description.
#' @param defaultScenario The model default scenario, a 
#' \code{\link{sdScenarioClass}} object without state variables. 
#' It should contain all the model variables initialized with default values 
#' that ensures the model simulation.
#' @param algebraicEquations A list with the model algebraic equations in 
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
#' @param initVars (Optional) An R-function that initialize or change the 
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
#' The return value of the \code{initVars} function should be a list containing 
#' all the function arguments, except the algebraic equations, named in the same 
#' way, e.g. \code{return(list(ct = ct, inp = inp, par = par, sw = sw))}.
#' @param globalFunctions A named list of extra functions that can be executed 
#' in the scope of any other function or algebraic equation defined in the 
#' model. They can be called by their names in the list.
#' @return A \code{\link{sdStaticModelClass}} object.
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
sdStaticModel <- function(id = NULL,
                          description = NULL,
                          defaultScenario = NULL,
                          algebraicEquations = NULL,
                          initVars = NULL,
                          globalFunctions = NULL) { 
  # create a new model
  model <- sdStaticModelClass$new(
    id = id,
    description = description,
    initVars = initVars,
    algebraicEquations = algebraicEquations,
    defaultScenario = defaultScenario,
    globalFunctions = globalFunctions)
  
  return(model)
}

#' Creates a Scenario Object
#' 
#' A factory function that creates a \code{\link{sdScenarioClass}} object that 
#' represents the variables 
#' and values that constitute a system environment, and that are used 
#' to compute the equations of a model. It also stores configurations for a 
#' simulation, e.g. the time sequence and integrator method.
#' 
#' The varible names are coerced to syntatically valid names following the 
#' criterias described in \code{\link{sdsim-LabelingRules}}.
#' 
#' To load a scenario from an EXCEL or XML file use the 
#' \code{\link{sdLoadScenario}} function.
#' 
#' To build a coupled scenario use the \code{\link{sdBuildCoupledScenario}} 
#' function.
#' 
#' @param id A character string with the scenario ID.
#' @param times A named list containing three elements to be passed to the
#' \code{\link{seq}} function: from - the simulation initial time, to - the 
#' simulation final time and by - the time step, increment of the sequence (e.g.
#' list(from = 0, to = 100, by = 1)).
#' @param method The default integrator to be used in the simulations, 
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
#' @param state A numeric list with the default initial state values 
#' for the ODE system. The state variables are used to describe the mathematical 
#' "state" of a dynamic system. The continuous rate of change of these variables 
#' is determined by the model \code{ode} function. All the 
#' elements in this list must be named. Or a data.frame following the 
#' guidelines in the Data.frame Format section.
#' @param constant A numeric list with the model constant variables. 
#' All the elements in the list must be named. Or a data.frame following the 
#' guidelines in the Data.frame Format section.
#' @param parameter A numeric list containing the parameters of the 
#' scenario. All the elements in this list must be named. Or a data.frame following the 
#' guidelines in the Data.frame Format section.
#' @param input A list containing the inputs of the scenario. All the
#' elements in the list must be named. Or a data.frame following the 
#' guidelines in the Data.frame Format section.
#' @param interpolation A list containing the interpolation methods for any time
#' series variable present in the input list. All the elements in the list must 
#' be named with the respective input time series variable name. See 
#' \code{\link{sdTemporalFunction}} for the complete list of available methods.
#' Missing if already present in the \code{input} data.frame, otherwise will 
#' overwrite it.
#' @param switch A list containing the switches of the scenario. All the
#' elements in the list must be named. Or a data.frame following the 
#' guidelines in the Data.frame Format section.
#' @param unit A list with the model variables units. Each element of
#' this list represents a variable (named with the variable name) and it's 
#' value is a string with the variable unit. Missing if variables were passed in
#' a data.frame or it will be concatenated with the given units in any 
#' data.frame.
#' @param description A list with the model variables descriptions. Each element 
#' of this list represents a variable (named with the variable name) and it's 
#' value is a string with the variable description. Missing if variables were 
#' passed in a data.frame or it will be concatenated with the given 
#' descriptions in any data.frame.
#' @param timeSeriesDirectory The directory where the time series inputs are 
#' stored (when passing the time series inputs via external files).
#' @param variableCol When using data.frames, the name of the column that 
#' contains the variable's names. Default is 'Variable'.
#' @param valueCol When using data.frames, the name of the column that contains 
#' the variable's values. Default is 'Value'.
#' @param unitCol When using data.frames, the name of the column that contains 
#' the variable's units. Default is 'Unit'.
#' @param descriptionCol When using data.frames, the name of the column that 
#' contains the variable's descriptions. Default is 'Description'.
#' @param interpolationCol When using data.frames, the name of the column of the
#' input time series that contains the interpolation methods. Default is
#' 'Interpolation'.
#' @section Data.frame Format: The data.frame of each type of variable must 
#' follow these guidelines:
#' 
#' \itemize{
#'   \item Have header
#'   \item Have 4 columns, with the default parameters labels as bellow: 
#'   \enumerate{
#'     \item Variable - the column with the variables name
#'     \item Value - the column with the variables value
#'     \item Unit - the column with the variables unit
#'     \item Description - the column with the variables description
#'   }
#'   With the exception of the \code{input} that should also contain a 
#'   fifth column, labeled 'Interpolation' containing the interpolation method 
#'   to be used in the time series variables. Each variable that have a valid
#'   method will be passed to the \code{\link{sdTemporalFunction}} to be 
#'   automatically converted into a temporal function.
#'   \item Each row of the data.frame defines a new variable.
#'   \item The default decimal point character is '.'
#'   \item Empty variable names and invalid values will be skipped 
#'   with a warning to the user.  
#'   \item All the leading and trailing whitespaces will be trimmed.
#' }
#' 
#' @return A \code{\link{sdScenarioClass}} object.
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
#' dummyScen <- sdScenario(id = "dummyScenario",
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
#' # state variables
#' st <- data.frame(Variable = c("s1", "s2"), 
#'                  Value = c(5,10),
#'                  Description = c("state var 1", "state var 2"),
#'                  Unit = c("meter", "meter / second"),
#'                  stringsAsFactors = FALSE)
#' 
#' # input variables
#' inp <- data.frame(Variable = c("i1", "ts1"), 
#'                  Value = c(10, "data.frame(Time = c(1, 5, 10), Value = c(5, 10, 20))"),
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
#' dummyScen <- sdScenario(id = "dummyScenario",
#'                         state = st, 
#'                         input = inp, 
#'                         constant = ct,
#'                         times = times,
#'                         method = method) 
#' print(dummyScen)
sdScenario <- function(id,
                       times = list(from = 0, to = 100, by = 1),
                       method = c("lsoda", "lsode", "lsodes", "lsodar", "vode", "daspk",
                                  "euler", "rk4", "ode23", "ode45", "radau", 
                                  "bdf", "bdf_d", "adams", "impAdams", "impAdams_d"),
                       state, 
                       constant,
                       parameter,
                       input,
                       interpolation,
                       switch,
                       unit,
                       description,
                       timeSeriesDirectory = "",
                       variableCol = "Variable", 
                       valueCol = "Value", 
                       unitCol = "Unit", 
                       descriptionCol = "Description", 
                       interpolationCol = "Interpolation") { 
  loadedScen <- list()
  
  # Update the variables from external files with the given arguments and
  # initialize the object fields
  if (!missing(id) && !is.null(id))
    loadedScen$id <- id
  
  if (!missing(times) && !is.null(times)) # set simulation time seq.
    loadedScen$times <- times
  
  if (!missing(method) && !is.null(method)) # set method
    loadedScen$method <- method
  
  if (!missing(state)) { # set state
    if (is.data.frame(state)) { 
      loadedScen$state <- ConvertDataFrameToList(state, 
                                                 variableCol = variableCol,
                                                 valueCol = valueCol)
      if (!all(state[[unitCol]] %in% ""))
        loadedScen$unit <- c(loadedScen$unit, 
                             ConvertDataFrameToList(
                               state, 
                               variableCol = variableCol,
                               valueCol = unitCol))
      if (!all(state[[descriptionCol]] %in% ""))
        loadedScen$description <- c(loadedScen$description, 
                                    ConvertDataFrameToList(
                                      state, 
                                      variableCol = variableCol,
                                      valueCol = descriptionCol))
    } else {
      loadedScen$state <- state
    }
  }
  
  if (!missing(constant)) { # set constant
    if (is.data.frame(constant)) { 
      loadedScen$constant <- ConvertDataFrameToList(constant, 
                                                    variableCol = variableCol,
                                                    valueCol = valueCol)
      if (!all(constant[[unitCol]] %in% ""))
        loadedScen$unit <- c(loadedScen$unit, 
                             ConvertDataFrameToList(constant, 
                                                    variableCol = variableCol,
                                                    valueCol = unitCol))
      if (!all(constant[[descriptionCol]] %in% ""))
        loadedScen$description <- c(loadedScen$description, 
                                    ConvertDataFrameToList(
                                      constant, 
                                      variableCol = variableCol,
                                      valueCol = descriptionCol))
    } else {
      loadedScen$constant <- constant
    }
  }
  
  if (!missing(input)) { # set input
    if (is.data.frame(input)) { 
      loadedScen$input <- ConvertDataFrameToList(input, 
                                                 variableCol = variableCol,
                                                 valueCol = valueCol)
      if (!all(input[[unitCol]] %in% ""))
        loadedScen$unit <- c(loadedScen$unit, 
                             ConvertDataFrameToList(input, 
                                                    variableCol = variableCol,
                                                    valueCol = unitCol))
      if (!all(input[[descriptionCol]] %in% ""))
        loadedScen$description <- c(loadedScen$description, 
                                    ConvertDataFrameToList(
                                      input, 
                                      variableCol = variableCol,
                                      valueCol = descriptionCol))
      if (!all(input[[interpolationCol]] %in% ""))
        loadedScen$interpolation <- ConvertDataFrameToList(
          input, 
          variableCol = variableCol,
          valueCol = interpolationCol)
    } else {
      loadedScen$input <- input
    }
  }
  
  if (!missing(interpolation) && !is.null(interpolation))
    loadedScen$interpolation <- interpolation
  
  if (!missing(parameter)) { # set parameter
    if (is.data.frame(parameter)) { 
      loadedScen$parameter <- ConvertDataFrameToList(parameter, 
                                                     variableCol = variableCol,
                                                     valueCol = valueCol)
      if (!all(parameter[[unitCol]] %in% ""))
        loadedScen$unit <- c(loadedScen$unit, 
                             ConvertDataFrameToList(
                               parameter, 
                               variableCol = variableCol,
                               valueCol = unitCol))
      if (!all(parameter[[descriptionCol]] %in% ""))
        loadedScen$description <- c(loadedScen$description, 
                                    ConvertDataFrameToList(
                                      parameter, 
                                      variableCol = variableCol,
                                      valueCol = descriptionCol))
      
    } else {
      loadedScen$parameter <- parameter
    }
  }
  
  if (!missing(switch)) { # set switch
    if (is.data.frame(switch)) { 
      loadedScen$switch <- ConvertDataFrameToList(switch, 
                                                  variableCol = variableCol,
                                                  valueCol = valueCol)
      if (!all(switch[[unitCol]] %in% ""))
        loadedScen$unit <- c(loadedScen$unit, 
                             ConvertDataFrameToList(
                               switch, 
                               variableCol = variableCol,
                               valueCol = unitCol))
      if (!all(switch[[descriptionCol]] %in% ""))
        loadedScen$description <- c(loadedScen$description, 
                                    ConvertDataFrameToList(
                                      switch, 
                                      variableCol = variableCol,
                                      valueCol = descriptionCol))
    } else {
      loadedScen$switch <- switch
    }
  }
  
  # set unit list
  if (!missing(unit) && !is.null(unit))
    loadedScen$unit <- loadedScen$unit <- c(loadedScen$unit, unit)
  
  # set description list
  if (!missing(description) && !is.null(description))
    loadedScen$description <- c(loadedScen$description, description)
  
  return(sdScenarioClass$new(id = loadedScen$id,
                             times = loadedScen$times, 
                             method = loadedScen$method,
                             state = loadedScen$state,
                             constant = loadedScen$constant,
                             parameter = loadedScen$parameter,
                             input = loadedScen$input,
                             interpolation = loadedScen$interpolation,
                             switch = loadedScen$switch,
                             unit = loadedScen$unit,
                             description = loadedScen$description,
                             timeSeriesDirectory = timeSeriesDirectory))
}

#' Loads a Scenario Object from External Files
#' 
#' A factory function that creates a \code{\link{sdScenarioClass}} object from 
#' XML or EXCEL files. Uses the \code{file} extension to determine it's format.
#' 
#' Use the \code{\link{sdExcelTemplate}} function to create an empty template 
#' for a sdsim scenario EXCEL file.
#' 
#' @param file A scenario file name in '.xlsx', '.xls' or '.xml' containing the 
#' format extension.
#' @param timeSeriesDirectory The directory where time series inputs are stored 
#' (when passing the time series inputs via external files).
#' @param stateSheet A character string with the name of the EXCEL sheet
#' that contains the state variables. Default is 'state'. (Only used if 
#' the file is a '.xlsx' or '.xls').
#' @param constantSheet A character string with the name of the EXCEL sheet
#' that contains the constant variables. Default is 'constant'. (Only used if 
#' the file is a '.xlsx' or '.xls').
#' @param inputSheet A character string with the name of the EXCEL sheet
#' that contains the input variables. Default is 'input'. (Only used if 
#' the file is a '.xlsx' or '.xls').
#' @param parameterSheet A character string with the name of the EXCEL 
#' sheet that contains the parameter variables. Default is 'parameter'. (Only 
#' used if the file is a '.xlsx' or '.xls').
#' @param switchSheet A character string with the name of the EXCEL sheet
#' that contains the switch variables. Default is 'switch'.(Only used if 
#' the file is a '.xlsx' or '.xls').
#' @param simulationSheet A character string containing the name of the EXCEL 
#' sheet that contains the simulation configuration variables. Default is 
#' 'simulation'. (Only used if the file is a '.xlsx' or '.xls').
#' @param variableCol A character string with the name of the column containing 
#' the variables names.
#' @param valueCol A character string with the name of the column containing 
#' the variables values.
#' @param unitCol A character string with the name of the column containing 
#' the variables units.
#' @param descriptionCol A character string with the name of the column 
#' containing the variables descriptions.
#' @param interpolationCol A character string with the name of the column 
#' containing the input time series variables interpolation method.
#' @section EXCEL Format: The EXCEL file representing a 
#' \code{\link{sdScenarioClass}} object must have one sheet per type of variable 
#' and a sheet for the simulation configurations, e.g. the \code{stateSheet}, 
#' \code{constantSheet}, \code{inputSheet}, \code{parameterSheet}, 
#' \code{switchSheet} and the \code{simulationSheet}. A sheet can be missing if 
#' the scenario do not have the corresponding type of variable.
#' 
#' Each sheet must follow this guidelines:
#' 
#' \itemize{
#'   \item Have header
#'   \item Have 4 columns, with the default parameters labels as bellow: 
#'   \enumerate{
#'     \item Variable - the column with the variables name
#'     \item Value - the column with the variables value
#'     \item Unit - the column with the variables unit
#'     \item Description - the column with the variables description
#'   }
#'   With the exception of the \code{inputSheet} that should also contain a 
#'   fifth column, labeled 'Interpolation' containing the interpolation method 
#'   to be used in the time series variables. Each variable that have a valid
#'   method will be passed to the \code{\link{sdTemporalFunction}} to be 
#'   automatically converted into a temporal function.
#'   
#'   \item Each row of the sheets defines a new variable. With exception to the
#'   \code{simulationSheet} that can only have the following 5 (five) hard coded
#'   variables rows, named as bellow: 
#'   \enumerate{
#'     \item id - A string with the scenario identification.
#'     \item from - the simulation initial time
#'     \item to - the simulation final time
#'     \item by - the simulation time step, increment of the sequence
#'     \item method - The default integrator to be used in the simulations, 
#' a string ("lsoda", "lsode", "lsodes","lsodar","vode", "daspk", "euler", 
#' "rk4", "ode23", "ode45", "radau", "bdf", "bdf_d", "adams", "impAdams" or 
#' "impAdams_d"). When running with support to events the given method must be 
#' one of the following routines, which have root-finding capability: 
#' \code{\link[deSolve]{lsoda}}, \code{\link[deSolve]{lsode}} or
#' \code{\link[deSolve]{radau}}; If the given method is different from any of 
#' these three routines the simulator will run with the default method 
#' "lsoda". See the \code{\link[deSolve]{ode}} and the \code{\link[deSolve]{events}} 
#' details section for more information.
#'   }
#'   
#'   \item The default decimal point character is '.'
#'   
#'   \item The sheet names must follow the defaults or be specified in the 
#'   arguments.
#'   
#'   \item Empty sheets, empty variable names and invalid values will be skipped 
#'   with a warning to the user. 
#'   
#'   \item All the leading and trailing whitespaces will be trimmed.
#' }
#' @return A \code{\link{sdScenarioClass}} object.
#' @examples 
#' ## Let's load a model from the repository, save it's default scenario in a 
#' # EXECEL file, change it and then reload it to use it in a different 
#' # simulation
#' 
#' ## Load the Bouncing Ball Model from the sdsim repository
#' bb <- sdLoadModel(file = "BouncingBall", repository = TRUE)
#' 
#' # save the bouncing ball default scenario to an EXCEL file
#' bb$defaultScenario$saveXlsx(file = "bb.xlsx")
#' 
#' ## Let's edit the the EXCEL file to define a new scenario where we are 
#' # throwing a tennis ball  from our hand to the ground
#' # To represent this scenario we need to edit the following 4 sheets:
#' # In the 'state' sheet:
#' #  - change the value of the variable 'height' to 1.5
#' #  - change the value of the variable 'speed' to -10 (negative indicating the 
#' # ball direction, to the ground)
#' # In the 'input' sheet:
#' #  - change the value of the variable 'k' to 0.75 (the tennis ball 
#' # coefficient of restitution when kicking in a tennis field)
#' # In the 'simulation' sheet: 
#' #  - change the value of the variable 'to' to 8 (it will stop faster)
#' #  - change the value of the variable 'by' to 0.001 (more precision)
#' #  - change the value of the variable 'id' to 'TennisBallKicking'
#' # Remove the 'constant' sheet (we will use the default value)
#' 
#' ## Save the modified EXCEL file and reload it
#' tennisBallScen <- sdLoadScenario(file = "bb.xlsx")
#' 
#' # Simulate the Bouncing Ball model with the new scenario and plot the result
#' outbbTennis <- sdSimulate(model = bb, scenario = tennisBallScen)
#' outbbTennis$plot()
sdLoadScenario <- function(file,
                           timeSeriesDirectory = "",
                           stateSheet = "state",
                           constantSheet = "constant",
                           inputSheet = "input",
                           parameterSheet = "parameter",
                           switchSheet = "switch",
                           simulationSheet = "simulation",
                           variableCol = "Variable", 
                           valueCol = "Value", 
                           unitCol = "Unit", 
                           descriptionCol = "Description", 
                           interpolationCol = "Interpolation") { 
  # Detect if file extension is equal ".xml" or to xlsx/xls
  isXml <- grepl("\\.([xX][mM][lL])$", file, perl = TRUE) 
  isXlsx <- grepl("\\.([xX][lL][sS][xX]|[xX][lL][sS])$", file, perl = TRUE)
  
  # check if file exists
  if (!file.exists(file)) 
    stop(sprintf(constructorsMsg$sdLoadScenario1, file))
  
  if (isXml) { 
    # read the file
    loadedScen <- tryCatch(
      XML::xmlParse(file),
      error = function(e) { 
        stop(sprintf(constructorsMsg$sdLoadScenario2, file, e))
      },
      warning = function(w) { 
        warning(sprintf(constructorsMsg$sdLoadScenario3,file,w))
      })
    
    # Check if file is a valid sdsim xml file containing a scenario XML
    sdsimprefix <- paste(readLines(file, n = 3), collapse = "\n")
    if (!grepl(pattern = "<\\?sdsim.*version.*\\?>", sdsimprefix, 
               ignore.case = TRUE) ||
        !(sdScenarioClass$classname %in% names(XML::xmlChildren(loadedScen))))
      stop(sprintf(constructorsMsg$sdLoadScenario2, file, ""))
    # else
    # {
    #   # valid prefix, now check version
    #   if (!grepl(pattern = paste0("(?<=version=\\')", 
    #                               sub("\\.","\\\\.", packageVersion("sdsim"))), 
    #              x = sdsimprefix, ignore.case = TRUE, perl = TRUE))
    #     warning("Load Scenario: The sdsim XML version is deprecated. ",
    #             "The current sdsim version is: ", packageVersion("sdsim"))
    # }
    
    loadedScen <- XML::xmlToList(loadedScen)
  } else if (isXlsx) { 
    loadedScen <- LoadModelScenario(file = file,
                                    dec = ".",
                                    stateSheet = stateSheet,
                                    constantSheet = constantSheet,
                                    inputSheet = inputSheet,
                                    parameterSheet = parameterSheet,
                                    switchSheet = switchSheet,
                                    simulationSheet = simulationSheet,
                                    variableCol = "Variable", 
                                    valueCol = "Value", 
                                    unitCol = "Unit", 
                                    descriptionCol = "Description", 
                                    interpolationCol = "Interpolation")
  } else {
    stop(sprintf(constructorsMsg$sdLoadScenario4, file))
    
  }
  
  return(sdScenarioClass$new(id = loadedScen$id,
                             times = loadedScen$times, 
                             method = loadedScen$method,
                             state = loadedScen$state,
                             constant = loadedScen$constant,
                             parameter = loadedScen$parameter,
                             input = loadedScen$input,
                             interpolation = loadedScen$interpolation,
                             switch = loadedScen$switch,
                             unit = loadedScen$unit,
                             description = loadedScen$description,
                             timeSeriesDirectory = timeSeriesDirectory))
}
# simulationSheet: variables: from, to, by, method, id
# variableCol = "Variable",
# valueCol = "Value",
# unitCol = "Unit",
# descriptionCol = "Description", 
# interpolationCol = "Interpolation"


# TODO: finish documentation
#' Create flow object
#'  
#'
#' This function converts written text to a list of variables in character 
#' format that contains the flow model. It also guarantees that the arguments to 
#' are adequated to be used by other sdsim functions.
#' 
#' @param flows 
#' @param flow_rate
#' @param st
#' @param boundaries
#' 
#' @return  
#'
#' @examples
#' flows <- sdFlow(
#'   connections = c("birth -> prey", 
#'                   "prey -> death",
#'                   "birth -> predator",
#'                   "predator -> death"),
#'   flow_rate = c(
#'     sdsim::sdEquationList(
#'       st$prey * par$a,
#'       par$b * st$prey * st$predator,
#'       par$delta * st$prey * st$predator,
#'       par$gamma * st$predator
#'     )
#'   ),
#'   boundaries = c("birth", "death"),
#'   st = c("prey", "predator")
#' )
#' scen <- sdScenario(
#'   id = "test",
#'   state = list(prey = 10,
#'                predator = 10),
#'   parameter = list(a = 2/3,
#'                    b = 4/3,
#'                    delta = 1,
#'                    gamma = 1),
#'   times = list(from = 0, to = 100, by = 0.1),
#'   method = "lsoda"
#' )
#' 
#' model <- sdOdeModel("test",
#'                     ode = flows,
#'                     defaultScenario = scen)
#' out <- sdSimulate(model)
#' plot(out)
sdFlow <- function(flows = NULL, flowRate = NULL, 
                   stocks = NULL, boundaries = c("boundary")) {
  sdFlowOdeClass$new(flows,
                     flowRate = flowRate,
                     stocks = stocks,
                     boundaries = boundaries)
}