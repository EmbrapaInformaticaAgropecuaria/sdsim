CreateFuncEval <-
  function(func,
           env,
           auxiliary,
           lastEvalTime,
           unlistReturn = F,
           storeAuxTrajectory = F,
           stNames = NULL) { 
    
    st <- env$st
    ct <- env$ct
    par <- env$par
    inp <- env$inp
    sw <- env$sw
    
    aux <- vector("list", length = length(auxiliary))
    names(aux) <- names(auxiliary)
    auxseq <- seq_along(auxiliary)
    timeSeries <- match(names(inp$fun_), names(inp))

    FuncEval <- function(t, st, parms) { 
      e <- globalenv()
      e$counter <- e$counter + 1
      st <- as.list(st)
      if(!is.null(stNames))
        names(st) <- stNames

      # evaluate time series and auxiliary varibles if the last evaluation time
      # is different than the current time
      if (lastEvalTime != t) {
        # compute the time series contained in the input
        inp[timeSeries] <<- lapply(inp$fun_, function(x) x(t))
        
        lastEvalTime <<- t
      }
      
      # evaluate the auxiliary variables and update the aux list
      for (var in auxseq)
        aux[[var]] <- eval(auxiliary[[var]])

      output <- func(t = t, st = st, ct = ct, par = par,
                     inp = inp, sw = sw, aux = aux)
      
      # Save aux trajectory
      if (storeAuxTrajectory)
        output <- c(output, aux)
      

      if (!unlistReturn)
        return(output)
      else
        return(unlist(output))
    }
    
    return(FuncEval)
  }

# A clousure that returns a function that concatenate the funcs return value
# at each time t. It pre evaluates the models time series, make the connections
# using the A transformation matrix and pre evaluates the auxiliary equations.
CreateCoupledFuncEval <- function(componentsId,
                                  funcs,
                                  conSt,
                                  conStInps,
                                  conAux,
                                  conAuxInps,
                                  compIndex,
                                  lenst,
                                  ct,
                                  par,
                                  inp,
                                  sw,
                                  auxiliary,
                                  storeAuxTrajectory = F) { 
  lastEvalTime <- -Inf
  
  modelseq <- seq_along(componentsId)
  
  aux <- eq <- vector("list", length = length(auxiliary))
  names(aux) <- names(eq) <- names(auxiliary)
  auxseq <- seq_along(auxiliary)
  timeSeries <- match(names(inp$fun_), names(inp))
  
  dState <- vector("numeric", length = lenst)
  dAux <- list()
  
  CoupledFuncEval <- function(t, state, parms) { 
    # stores the model definitions result
    st <- as.list(state)
    
    # evaluate time series varibles if the last evaluation time
    # is different than the current time
    if (lastEvalTime != t) { 
      # compute the time series in the inputs
      inp[timeSeries] <<- lapply(inp$fun_, function(x) x(t))
      
      lastEvalTime <<- t
    }
    # make the connections using the transformation st connection matrix
    #inpConnection <- as.list(conSt %*% state)
    inp[conStInps] <<- st[conSt]
    
    # evaluate the auxiliary variables and update the aux list
    for (var in auxseq)
      eq[[var]] <<- aux[[var]] <<- eval(auxiliary[[var]])
    
    inp[conAuxInps] <<- aux[conAux]
    
    # run the components model definitions
    for (i in modelseq) { 
      # run the model definition
      mDef <- funcs[[i]](t = t,
                         st = st[compIndex[["st"]][[componentsId[[i]]]]],
                         ct = ct[compIndex[["ct"]][[componentsId[[i]]]]],
                         par = par[compIndex[["par"]][[componentsId[[i]]]]],
                         inp = inp[compIndex[["inp"]][[componentsId[[i]]]]],
                         sw = sw[compIndex[["sw"]][[componentsId[[i]]]]],
                         aux = aux[compIndex[["aux"]][[componentsId[[i]]]]])
      
      # concatenate the states derivatives
      dState[compIndex[["st"]][[componentsId[[i]]]]] <- mDef[[1]]
      # concatenate the global auxiliary values
      mAux <- mDef[-1]
      
      if (length(mAux) > 0) { # there is auxiliary values
        names(mAux)[names(mAux) %in% ""] <- "noname"
        names(mAux) <- paste0(componentsId[[i]], ".", names(mAux))
        dAux <- c(dAux, mAux)
      }
    }
    
    # Save aux trajectory
    if (storeAuxTrajectory) 
      return(list(dState, dAux, aux))
    else
      return(list(dState, dAux))
  }
  
  return(CoupledFuncEval)
}

CreateCoupledRootEventFunc <-
  function(componentsId,
           rootFuncs = NULL,
           eventFuncs = NULL,
           conSt,
           conStInps,
           conAux,
           conAuxInps,
           compIndex,
           ct,
           par,
           inp,
           sw,
           auxiliary) { 
    triggedEvents <- c()  # stores which models trigged events
    lastEvalTime <- -Inf
    
    aux <- eq <- vector("list", length = length(auxiliary))
    names(aux) <- names(eq) <- names(auxiliary)
    auxseq <- seq_along(auxiliary)
    
    timeSeries <- match(names(inp$fun_), names(inp))
    
    CoupledRootEval <- function(t, state, parms) { 
      triggedEvents <<- c()
      modelRoots <- roots <- c() # the roots returned by all models
      st <- as.list(state)
      
      # evaluate time series varibles if the last evaluation time
      # is different than the current time
      if (lastEvalTime != t) { 
        # compute the time series in the inputs
        inp[timeSeries] <<- lapply(inp$fun_, function(x) x(t))
        
        lastEvalTime <<- t
      }
      
      # make the connections using the vectors
      inp[conStInps] <<- st[conSt]
      
      # evaluate the auxiliary variables and update the aux list
      for (var in auxseq)
        eq[[var]] <<- aux[[var]] <<- eval(auxiliary[[var]])
      
      inp[conAuxInps] <<- aux[conAux]
      
      # run the components root funcs
      for (modelId in componentsId) { 
        if (is.function(rootFuncs[[modelId]]))
          modelRoots <- rootFuncs[[modelId]](
            t = t,
            st = st[compIndex[["st"]][[modelId]]],
            ct = ct[compIndex[["ct"]][[modelId]]],
            par = par[compIndex[["par"]][[modelId]]],
            inp = inp[compIndex[["inp"]][[modelId]]],
            sw = sw[compIndex[["sw"]][[modelId]]],
            aux = aux[compIndex[["aux"]][[modelId]]])
        else if (is.data.frame(rootFuncs[[modelId]])) { 
          if (any(rootFuncs[[modelId]]$time == t))
            modelRoots <- 0
          else 
            modelRoots <- 1
        } else if (is.numeric(rootFuncs[[modelId]])) { 
          modelRoots <- rootFuncs[[modelId]] - t
        }
        
        roots <- c(roots, modelRoots)
        
        # save the models that trigged an event
        if (any(modelRoots == 0))
          triggedEvents <<- c(triggedEvents, modelId)
      }
      
      return(roots)
    }
    
    CoupledEventEval <- function(t, state, parms) { 
      st <- as.list(state)
      
      if (!is.null(triggedEvents) || length(triggedEvents) > 0) { 
        triggedEvents <<- unique(triggedEvents)
        
        for (modelId in triggedEvents) { 
          if (!is.null(eventFuncs[[modelId]])) { 
            # run event
            if (is.function(eventFuncs[[modelId]])) { 
              newState <- eventFuncs[[modelId]](
                t = t,
                st = st[compIndex[["st"]][[modelId]]],
                ct = ct[compIndex[["ct"]][[modelId]]],
                par = par[compIndex[["par"]][[modelId]]],
                inp = inp[compIndex[["inp"]][[modelId]]],
                sw = sw[compIndex[["sw"]][[modelId]]],
                aux = aux[compIndex[["aux"]][[modelId]]])
              
              # update state variables
              st[compIndex[["st"]][[modelId]]] <- newState
            } else if (is.data.frame(eventFuncs[[modelId]])) { 
              trigged <- eventFuncs[[modelId]][eventFuncs[[modelId]]$time == t,]
              
              for (i in 1:nrow(trigged)) { 
                st[trigged[i,1]] <- switch(
                  trigged[[i, 4]],
                  replace = trigged[[i, 3]],
                  add = st[[trigged[i,1]]] + trigged[[i, 3]],
                  multiply = st[[trigged[i,1]]] * trigged[[i, 3]] )
              }
            }
          }
        }
        
        triggedEvents <<- c()
      }
      
      return(unlist(st))
    }
    
    return(list(coupledRootFunc = CoupledRootEval, 
                coupledEventFunc = CoupledEventEval))
  }

#' Simulate System Dynamics Models and Static Models
#'
#' Simulates a \code{model} using it's default scenario merged
#' with the given \code{scenario}. A wrapper around the 
#' \code{\link[deSolve]{ode}} solver.
#' 
#' If performance is crucial remember to guarantee that the model is already 
#' verified and the following logical parameters are set to FALSE: 
#' storeAuxTrajectory, storeTimeSeriesTrajectory, verbose.
#'
#' @param model A \code{\link{sdOdeModelClass}}, a 
#' \code{\link{sdCoupledModelClass}} or a \code{\link{sdStaticModelClass}} 
#' object.
#' @param scenario A \code{\link{sdScenarioClass}} object or a character string 
#' with a scenario XML or EXCEL file name. 
#' 
#' If the \code{model} is a \code{\link{sdCoupledModelClass}} object the 
#' \code{scenario} must be a coupled scenario object (created with the 
#' \code{\link{sdBuildCoupledScenario}} function), or a list of 
#' \code{\link{sdScenarioClass}} objects and/or character strings with a 
#' scenario XML or EXCEL file name - the elements of this list must be named 
#' with the component ID that will use it.
#' @param from If not missing, overwrites the starting value of the time 
#' sequence. Of length 1.
#' @param to If not missing, overwrites the end (maximal) value of the time 
#' sequence. Of length 1.
#' @param by If not missing, overwrites the increment of the time sequence. 
#' A number of length 1.
#' @param method If not missing, overwrites the integration method.
#' 
#' The integrator to be used in the simulation, a string
#' ("lsoda", "lsode", "lsodes","lsodar","vode", "daspk", "euler",
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
#' @param events logical: if \code{TRUE} run the simulation with support to
#' events (only if the \code{model} have a root specification); if \code{FALSE} 
#' do not run the simulation with support to events. Default is \code{TRUE}.
#' @param maxroots When events = TRUE and events are triggered by a root, the 
#' maximal number of times at with a root is found and that are kept; defaults 
#' to 100. If the number of roots > maxroot, then only the first maxroot will be 
#' outputted.
#' @param terminalroot When events = TRUE and events are triggered by a root, 
#' the default is that the simulation continues after the event is executed. 
#' In terminalroot, we can specify which roots should terminate the simulation.
#' @param ties When events = TRUE and events are specified by a data.frame and 
#' are "ordered", set to "ordered". The default is "notordered". 
#' This will save some computational time.
#' @param storeAuxTrajectory logical: if \code{TRUE} record the
#' \code{model} auxiliary equations trajectories if any (good for visualization,
#' but loses performance). Default is \code{TRUE}.
#' @param storeTimeSeriesTrajectory logical: if \code{TRUE} builds the
#' time series trajectories if any (good for visualization, but loses
#' performance). Default is \code{FALSE}.
#' @param verbose Logical: If \code{TRUE} provides additional details as to what 
#' the computer is doing. Default is \code{FALSE}.
#' @return A \code{\link{sdOutput}} object initialized with the simulation 
#' trajectories.
#' @examples 
#' # Load the Bouncing Ball model from the sdsim repository
#' bb <- sdLoadModel(file = "BouncingBall", repository = TRUE)
#' 
#' # simulate the model with validation and plot the results
#' outbb <- sdSimulate(model = bb, verbose = TRUE)
#' outbb$plot("height speed", multipleYAxis = TRUE, units = TRUE)
#' 
#' # simualte the Bouncing Ball model in a different scenario with the 
#' # coeficient of restitution equals 0.5 and a shorter time sequence
#' hardBallScen <- sdScenario(id = "hardBall", 
#'                            times = list(to = 5),
#'                            input = list(k = 0.5))
#' outbbHard <- sdSimulate(model = bb, scenario = hardBallScen)
#' plot(outbbHard)
sdSimulatorClass <- R6::R6Class(
  classname = "sdSimulator",
  
  public = list(
    initialize = function(model,
                          scenario = NULL,
                          from = NULL,
                          to = NULL,
                          by = NULL,
                          method = NULL
                          ) {
      # save model
      if (missing(model))
        stop(sprintf(sdSimulatorMsg$sdSimulate))
      private$pModel <- model
      
      #coupled Model verifications
      if (inherits(model, sdCoupledModelClass$classname)) {
        # Try to build the coupled model if it is not built
        if (!model$isBuilt) { 
          model$buildCoupledModel(from = from,
                                  to = to,
                                  by = by,
                                  method = method)
          
          if (!model$isBuilt)
            stop(sprintf(sdSimulatorMsg$sdSimulateCoupled1,model$id))
        }
        
        if (length(model$componentsEquations) == 0 && 
            length(model$componentsAux) == 0)
          stop(sprintf(sdSimulatorMsg$sdSimulateCoupled0,model$id))
        
        # convert list of scenarios to coupled scenario
        if (is.list(scenario))
          scenario <- sdBuildCoupledScenario(id = "coupledScen", 
                                             scenarios = scenario)
      }
      
      # get the simulation scenario
      if (!is.null(model$defaultScenario)) { 
        # get the model scenario 
        defaultScenario <- model$defaultScenario$clone(deep = TRUE)
        
        # overwrite default variables with the given scenario values
        if (!is.null(scenario)) { 
          if (is.character(scenario))
            scenario <- sdLoadScenario(file = scenario)
          
          if (inherits(scenario, sdScenarioClass$classname)) {
            private$pSimScenario <- mergeScenarios(defaultScenario, scenario)
            private$pAltScenario <- scenario
          }
          else
            warning(sprintf(sdSimulatorMsg$sdSimulateAtomic6,
                            model$id, typeof(scenario)))
        } else {
          private$pSimScenario <- model$defaultScenario
        }
      } else if (!is.null(scenario)) { 
        if (is.character(scenario))
          scenario <- sdLoadScenario(file = scenario)
        
        if (inherits(scenario, sdScenarioClass$classname)) {
          private$pSimScenario <- scenario
        }
        else
          stop(sprintf(sdSimulatorMsg$sdSimulateModel, "Ode", model$id), 
               call. = FALSE)
      } else {
        stop(sprintf(sdSimulatorMsg$sdSimulateModel, "Ode", model$id), 
             call. = FALSE)
      }
      
      private$pTimes <- private$pSimScenario$times
      
      # Override scenario times with the function parameters
      if (!is.null(from))
        private$pTimes$from <- from
      if (!is.null(to))
        private$pTimes$to <- to
      if (!is.null(by))
        private$pTimes$by <- by

      # verify data
      if (is.null(private$pTimes) || !all(c("from", "to", "by") %in% names(private$pTimes)))
        stop(sprintf(sdSimulatorMsg$sdSimulateAtomic2,private$pModel$id))
      else if (!(abs(private$pTimes$to - private$pTimes$from) >= abs(private$pTimes$by) && 
                 (private$pTimes$to - private$pTimes$from)*private$pTimes$by > 0)) # invalid time sequence
        stop(sprintf(sdSimulatorMsg$sdSimulateAtomic0,private$pModel$id))
      
      
      # Override scenario method with the function parameters
      if (!inherits(private$pModel, sdStaticModelClass$classname)) {
        private$pMethod <- private$pSimScenario$method
        
        if (!is.null(method))
          private$pMethod <- method
        
        if (is.null(private$pMethod)) { 
          warning(sprintf(sdSimulatorMsg$sdSimulateAtomic5,private$pModel$id))
          private$pMethod <- "lsoda"
        }
      }
  
    },
    print = function() {
      cat(indent("$model", indent = 4), sep = "\n")
      cat(indent(capture.output(private$pModel), indent = 4), 
          sep = "\n")
      cat(indent("$times", indent = 4), sep = "\n")
      timesDF <- data.frame(
        Variable = c(names(private$pTimes)), 
        Value = c(unlist(private$pTimes, use.names = FALSE)), 
        stringsAsFactors = FALSE)
      if (length(timesDF) > 0) { 
        cat(indent(paste(capture.output(timesDF), 
                         collapse =  "\n"), indent = 4))
        cat("\n")
      }
      cat("\n")
      cat(indent("$method", indent = 4), sep = "\n")
      cat(indent(private$pMethod, indent = 4), sep = "\n")
      cat("\n")
      cat(indent("$simScenario", indent = 4), sep = "\n")
      cat(indent(capture.output(private$pSimScenario), indent = 4), 
          sep = "\n")
      if(!is.null(private$pAltScenario)) {
        cat(indent("$altScenario", indent = 4), sep = "\n")
        cat(indent(capture.output(private$pAltScenario), indent = 4), 
            sep = "\n")
      }
        
    },
    initModel = function() {
      private$pOdeEnv <- new.env(parent = emptyenv())
      
      initVars <- private$pModel$initVars
      postProcess <- private$pModel$postProcess
      trigger <- private$pModel$trigger
      event <- private$pModel$event
      auxiliary <- private$pModel$aux
      
      # Get variables from default scenario
      st <- private$pSimScenario$state
      ct <- private$pSimScenario$constant
      par <- private$pSimScenario$parameter
      inp <- private$pSimScenario$input
      sw <- private$pSimScenario$switch
      
      # run the initVars function
      if (!is.null(initVars)) { 
        modelInit <- initVars(st = st,
                              ct = ct,
                              par = par,
                              inp = inp,
                              sw = sw,
                              aux = auxiliary)
        st <- modelInit$st
        ct <- modelInit$ct
        par <- modelInit$par
        inp <- modelInit$inp
        sw <- modelInit$sw
      }
      
      assign("st", st, private$pOdeEnv)
      assign("ct", ct, private$pOdeEnv)
      assign("par", par, private$pOdeEnv)
      assign("inp", inp, private$pOdeEnv)
      assign("sw", sw, private$pOdeEnv)
      
      private$pCurrTime <- private$pTimes$from
      private$pCurrState <- private$pSimScenario$state
      
      private$pOutput <- sdOutputClass$new(
        outTrajectory = unlist(private$pCurrState, use.names = F),
        auxTrajectory = NULL,
        timeSeriesTrajectory = NULL,
        model = private$pModel,
        scenario = private$pSimScenario,
        diagnostics = NULL,
        postProcessOut = NULL)
      
      # verify state variables
      if (is.null(st) || length(st) == 0)
        stop(sprintf(sdSimulatorMsg$sdSimulateAtomic1,model$id))
      
      environment(CreateFuncEval) <- private$pModel$modelEnvironment

      private$pOde <-
        CreateFuncEval(func = private$pModel$ode,
                       private$pOdeEnv,
                       auxiliary = auxiliary,
                       lastEvalTime = (private$pCurrTime - 1), # TODO: mudar para nulo?
                       storeAuxTrajectory = T,
                       unlistReturn = T,
                       stNames = names(st))
    },
    runSimulation = function(events = TRUE,
                        maxroots = 100,
                        terminalroot = NULL,
                        ties = "notordered",
                        storeAuxTrajectory = T,
                        storeTimeSeriesTrajectory = F,
                        verbose = F) {
      # If the model is atomic
      if (inherits(private$pModel, sdOdeModelClass$classname)) {
        private$pOutput <- runOdeSimulation(private$pOdeEnv, private$pModel, private$pSimScenario, private$pTimes$from, private$pTimes$to, 
                         private$pTimes$by, private$pMethod, events, maxroots, terminalroot,
                         ties, storeAuxTrajectory, storeTimeSeriesTrajectory)
      } 
      #if the model is static
      else if (inherits(private$pModel, sdStaticModelClass$classname)) {
        runStaticSimulation(private$pModel, private$pSimScenario, private$pTimes$from, private$pTimes$to, 
                            private$pTimes$by, private$pMethod, storeTimeSeriesTrajectory, verbose)
      } 
      #if the model is coupled
      else if (inherits(private$pModel, sdCoupledModelClass$classname)) {
        runCoupledSimulation(private$pModel, private$pSimScenario, private$pTimes$from, private$pTimes$to, 
                             private$pTimes$by, private$pMethod, events, maxroots,
                             storeAuxTrajectory, storeTimeSeriesTrajectory, verbose)
      } else {
        stop(sprintf(sdSimulatorMsg$sdSimulate2))
      }
    },
    runStep = function(time) {

      # If the model is atomic
      if (inherits(private$pModel, sdOdeModelClass$classname)) {
        # If time is smaller than current time
        if(time - private$pCurrTime > 0) {
          out <- sdsim::oneStep(ode = private$pOde, from = private$pCurrTime, to = time, state = private$pCurrState)

          # Save state trajectory 
          private$pOutput$UpdateOutTraj(out$state)
          private$pOdeEnv$st <- setNames(as.list(out$state), names(private$pCurrState))

          # Save auxiliary trajectory
          if(!is.null(out$aux))
            private$pOutput$UpdateAuxTraj(out$aux)

          # Update current time and current state
          private$pCurrTime <- time
          private$pCurrState <- setNames(as.list(out$state), names(private$pCurrState))
        } else if(time == private$pCurrTime) {
          # TODO
        } else {
          # TODO
        }
        
      }
      #if the model is static
      else if (inherits(private$pModel, sdStaticModelClass$classname)) {

      } 
      #if the model is coupled
      else if (inherits(private$pModel, sdCoupledModelClass$classname)) {

      } else {
        stop(sprintf(sdSimulatorMsg$sdSimulate2))
      }
      
    },
    runAllSteps = function() {
      time <- seq(private$pTimes$from, private$pTimes$to, private$pTimes$by)
      out <- sdsim::allSteps(private$pOde, time, private$pCurrState)
      private$pOutput$SetOutTraj(out$state)
    }
  ),
  active = list(
    model = function() {
      return(private$pModel)
    },
    times = function() {
      return(private$pTimes)
    },
    method = function() {
      return(private$pMethod)
    },
    altScenario = function() {
      return(private$pAltScenario)
    },
    simScenario = function() {
      return(private$pSimScenario)
    },
    output = function() {
      return(private$pOutput)
    }
  ),
  private = list(
    pModel = NULL,
    pTimes = NULL,
    pMethod = NULL,
    pAltScenario = NULL,
    pSimScenario = NULL,
    pOutput = NULL,
    pCurrTime = NULL,
    pCurrState = NULL,
    pOdeEnv = NULL,
    pOde = NULL
  )
)

runOdeSimulation <- function(env, model,
                             scenario = NULL,
                             from = NULL,
                             to = NULL,
                             by = NULL,
                             method = NULL,
                             events = TRUE,
                             maxroots = 100,
                             terminalroot = NULL,
                             ties = "notordered",
                             storeAuxTrajectory = T,
                             storeTimeSeriesTrajectory = F) {
  
  # Get model functions
  initVars <- model$initVars
  postProcess <- model$postProcess
  trigger <- model$trigger
  event <- model$event
  auxiliary <- model$aux

  
  environment(CreateFuncEval) <- model$modelEnvironment
  
  odeEval <-
    CreateFuncEval(func = model$ode,
                   env,
                   auxiliary = auxiliary,
                   lastEvalTime = (from - 1), # TODO: mudar para nulo?
                   storeAuxTrajectory = storeAuxTrajectory)
  
  # Run simulation without root function, data frame or times vector
  if (!events || is.null(trigger) ||
      (!is.function(trigger) &&
       !is.data.frame(trigger) && 
       !is.numeric(trigger))) { 
    # Run simulation without support to events
    outTrajectory <- deSolve::ode(
      y = unlist(env$st),
      times = seq(from, to, by),
      parms = NULL,
      func = odeEval,
      method = method
    )
  } else { # Run simulation with support to events
    if (is.function(trigger)) { 
      # check if the method support events
      if (!identical(method, deSolve::radau) &&
          !identical(method, deSolve::lsoda) &&
          !identical(method, deSolve::lsode) &&
          (!is.vector(method) ||
           !method %in% c("lsoda", "lsode", "radau"))) { 
        warning(sprintf(sdSimulatorMsg$sdSimulateAtomic3,model$id))
        method <- "lsoda"
      }
      
      triggerEval <-
        CreateFuncEval(func = trigger, ct = ct, par = par, 
                       inp = inp, sw = sw, auxiliary = auxiliary, 
                       lastEvalTime = (from - 1))
      
      if (is.function(event)) { 
        
        # EVENTS func triggered by a root function
        eventEval <- CreateFuncEval(event,
                                            ct, par, inp, sw, 
                                            auxiliary = auxiliary,
                                            lastEvalTime = (from - 1),
                                            unlistReturn = T)
        
        outTrajectory <- deSolve::ode(
          y = unlist(env$st),
          times = seq(from, to, by),
          func = odeEval,
          parms = NULL,
          rootfunc = triggerEval,
          events = list(
            func = eventEval, # TODO: da pra passar isso como nulo p/ evitar chamar o ode 2x?
            root = T,
            maxroots = maxroots,
            terminalroot = terminalroot),
          method = method
        )
      } else {
        # no event func, stop in the first root func
        outTrajectory <- deSolve::ode(
          y = unlist(env$st),
          times = seq(from, to, by),
          func = odeEval,
          parms = NULL,
          rootfunc = triggerEval,
          events = list(
            root = T,
            maxroots = maxroots,
            terminalroot = terminalroot
          ),
          method = method
        )
      }
    } else if (is.numeric(trigger) &&
               is.function(event)) { 
      # check if the method support events
      if (!identical(method, deSolve::radau) &&
          !identical(method, deSolve::lsoda) &&
          !identical(method, deSolve::lsode) &&
          (!is.vector(method) ||
           !method %in% c("lsoda", "lsode", "radau"))) { 
        warning(sprintf(sdSimulatorMsg$sdSimulateAtomic3,model$id))
        method <- "lsoda"
      }
      # events in a function with the triggers times in trigger
      eventEval <-
        CreateFuncEval(event,
                       ct,
                       par,
                       inp,
                       sw,
                       auxiliary = auxiliary,
                       lastEvalTime = (from - 1),
                       unlistReturn = T)
      
      outTrajectory <- deSolve::ode(
        y = unlist(env$st),
        times = seq(from, to, by),
        func = odeEval,
        parms = NULL,
        events = list(func = eventEval,
                      time = trigger),
        method = method)
    } else if (is.data.frame(trigger)) { 
      # check if the method support events
      if (!identical(method, deSolve::radau) &&
          !identical(method, deSolve::lsoda) &&
          !identical(method, deSolve::lsode) &&
          (!is.vector(method) ||
           !method %in% c("lsoda", "lsode", "radau"))) { 
        warning(sprintf(sdSimulatorMsg$sdSimulateAtomic3,model$id))
        method <- "lsoda"
      }
      outTrajectory <- deSolve::ode(
        y = unlist(env$st),
        times = seq(from, to, by),
        func = odeEval,
        parms = NULL,
        events = list(data = trigger,
                      ties = ties),
        method = method)
    } else { # run without events / maybe no event func for the time vector
      outTrajectory <- deSolve::ode(
        y = unlist(env$st),
        times = seq(from, to, by),
        parms = NULL,
        func = odeEval,
        method = method)
    }
  }
  
  diagnostics <-
    paste(capture.output(deSolve::diagnostics(outTrajectory)), 
          collapse = "\n")
  
  # Calculate time series trajectory
  if (storeTimeSeriesTrajectory && length(inp$interpolation_) > 0) { 
    tsTrajectory <- data.frame(time = outTrajectory[, "time"])
    
    for (x in inp$fun_)
      tsTrajectory <-
        cbind(tsTrajectory, as.numeric(x(tsTrajectory[, "time"])))
    
    colnames(tsTrajectory) <- c("time", names(inp$fun_))
  } else { 
    tsTrajectory <- NULL
  }
  
  if (storeAuxTrajectory && length(auxiliary) > 0) { 
    # separate the aux trajectory from the output trajectory
    # Auxiliaries begin at time column + length(state) + 1
    iAuxBegin <- 2 + length(env$st)
    outTrajectory <- as.data.frame(outTrajectory)
    auxTrajectory <- outTrajectory[, c(1, iAuxBegin:ncol(outTrajectory))]
    outTrajectory <- outTrajectory[, 1:(iAuxBegin - 1)]
  } else { 
    outTrajectory <- as.data.frame(outTrajectory)
    auxTrajectory <- NULL
  }
  
  # run post process func
  postProcess <- NULL
  if (!is.null(postProcess))
    postProcess <- tryCatch(
      postProcess(outTrajectory,
                      auxTrajectory, tsTrajectory,
                      ct, par, inp, sw),
      error = function(e) { 
        warning(sprintf(sdSimulatorMsg$sdSimulateAtomic4, model$id, e))
        return(NULL)
      })
  
  # Assemble output object
  output <- sdOutputClass$new(
    outTrajectory = outTrajectory,
    auxTrajectory = auxTrajectory,
    timeSeriesTrajectory = tsTrajectory,
    model = model,
    scenario = scenario,
    diagnostics = diagnostics,
    postProcessOut = postProcess)
  
  return(output)
}

runStaticSimulation <- function(model,
                                scenario = NULL,
                                from = NULL,
                                to = NULL,
                                by = NULL,
                                method = NULL,
                                storeTimeSeriesTrajectory = F,
                                verbose = F) {

  # Get model attributes
  equations <- model$algebraicEquations
  initVars <- model$initVars
  globalfuns <- model$globalFunctions
  
  # stop if model is empty
  if (length(equations) == 0)
    stop(sprintf(sdSimulatorMsg$sdSimulateStatic0,model$id))
  
  if (!model$isVerified)
    model$verifyModel(scenario, verbose = verbose)
  
  # Get variables from default scenario
  ct <- defaultScenario$constant
  par <- defaultScenario$parameter
  inp <- defaultScenario$input
  sw <- defaultScenario$switch

  if (!is.null(initVars)) { 
    modelInit <-
      initVars(ct = ct,
               par = par,
               inp = inp,
               sw = sw,
               eq = eq)
    ct <- modelInit$ct
    par <- modelInit$par
    inp <- modelInit$inp
    sw <- modelInit$sw
  }
  
  
  if (length(globalfuns) > 0)
    auxenv <- environment(globalfuns[[1]])
  else
    auxenv <- environment()
  
  eqTrajectory <- c()
  tsTrajectory <- NULL
  eq <- list()
  if (length(inp$fun_) > 0) { 
    timeSeries <- which(names(inp) %in% names(inp$fun_))
    for (t in seq(times$from, times$to, times$by)) { 
      # compute the input time Series values 
      inp[timeSeries] <- lapply(inp$fun_, function(x) x(t))
      
      for (equationsVar in names(equations))
        eq[[equationsVar]] <- eval(equations[[equationsVar]],
                                   enclos = auxenv)
      
      # Concatenate the equations trajectory
      eqTrajectory <- rbind(eqTrajectory, c(time = t, unlist(eq)))
      
      # Concatenate the time series trajectory
      if (storeTimeSeriesTrajectory)
        tsTrajectory <- rbind(tsTrajectory, 
                              c(time = t, 
                                unlist(inp[timeSeries])))
    }
    eqTrajectory <- as.data.frame(eqTrajectory, row.names = NULL)
    
    if (storeTimeSeriesTrajectory)
      tsTrajectory <- as.data.frame(tsTrajectory, row.names = NULL)
  } else { # no time series, output is constant
    for (equationsVar in names(equations))
      eq[[equationsVar]] <- eval(equations[[equationsVar]],
                                 envir = auxenv)
    eqTrajectory <- as.data.frame(rbind(eqTrajectory, 
                                        c(time = times$from, unlist(eq))), 
                                  row.names = NULL)
  }
  
  # Assemble output object
  output <- sdOutputClass$new(
    outTrajectory = eqTrajectory,
    auxTrajectory = NULL,
    timeSeriesTrajectory = tsTrajectory,
    model = model,
    scenario = scenario,
    diagnostics = NULL,
    postProcessOut = NULL)
  
  return(output)
}

runCoupledSimulation <- function(model,
                                 scenario = NULL,
                                 from = NULL,
                                 to = NULL,
                                 by = NULL,
                                 method = NULL,
                                 events = TRUE,
                                 maxroots = 100,
                                 storeAuxTrajectory = T,
                                 storeTimeSeriesTrajectory = F,
                                 verbose = F){


  if (!model$isVerified)
    model$verifyModel(scenario = scenario, verbose = verbose)
  
  # Get model functions
  componentsEquations <- model$componentsEquations
  componentsInitVars <- model$componentsInitVars
  componentsPostProcessVars <- model$componentsPostProcessVars
  componentsTrigger <-
    model$componentsTrigger
  componentsEvent <- model$componentsEvent
  aux <- model$componentsAux
  componentsId <- model$componentsId
  
  # get model variables
  st <- scenario$state
  ct <- scenario$constant
  par <- scenario$parameter
  inp <- scenario$input
  sw <- scenario$switch
  
  # Run the model Init Vars
  modelInit <- list()
  for (modelId in componentsId) { 
    # run the init vars
    if (!is.null(componentsInitVars[[modelId]])) { 
      if (inherits(model$components[[modelId]], sdOdeModelClass$classname)) {
        modelInitVars <-
          componentsInitVars[[modelId]](
            st = st[model$indexComponents$st[[modelId]]],
            ct = ct[model$indexComponents$ct[[modelId]]],
            par = par[model$indexComponents$par[[modelId]]],
            inp = inp[model$indexComponents$inp[[modelId]]],
            sw = sw[model$indexComponents$sw[[modelId]]],
            aux = aux[model$indexComponents$aux[[modelId]]])
      } else if (inherits(model$components[[modelId]], 
                          sdStaticModelClass$classname)) {
        modelInitVars <-
          componentsInitVars[[modelId]](
            ct = ct[model$indexComponents$ct[[modelId]]],
            par = par[model$indexComponents$par[[modelId]]],
            inp = inp[model$indexComponents$inp[[modelId]]],
            sw = sw[model$indexComponents$sw[[modelId]]],
            eq = aux[model$indexComponents$aux[[modelId]]])
      } else {
        next()
      }
      
      # concatenate the initVars return
      modelInit$st <- c(modelInit$st, modelInitVars$st)
      modelInit$ct <- c(modelInit$ct, modelInitVars$ct)
      modelInit$par <- c(modelInit$par, modelInitVars$par)
      modelInit$inp <- c(modelInit$inp, 
                         modelInitVars$inp[!(names(modelInitVars$inp) %in% 
                                               c("interpolation_", "fun_"))])
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
  
  # only static components
  if (length(componentsEquations) == 0) { 
    eqTrajectory <- c()
    tsTrajectory <- NULL
    eq <- list()
    if (length(inp$fun_) > 0) { 
      timeSeries <- which(names(inp) %in% names(inp$fun_))
      for (t in seq(from, to, by)) { 
        # compute the input time Series values 
        inp[timeSeries] <- lapply(inp$fun_, function(x) x(t))
        
        for (var in names(aux))
          eq[[var]] <- aux[[var]] <- eval(aux[[var]], enclos = model$modelEnv)
        
        # Concatenate the equations trajectory
        eqTrajectory <- rbind(eqTrajectory, c(time = t, unlist(eq)))
        
        # Concatenate the time series trajectory
        if (storeTimeSeriesTrajectory)
          tsTrajectory <- rbind(tsTrajectory, 
                                c(time = t, unlist(inp[timeSeries])))
      }
      eqTrajectory <- as.data.frame(eqTrajectory, row.names = NULL)
      
      if (storeTimeSeriesTrajectory)
        tsTrajectory <- as.data.frame(tsTrajectory, row.names = NULL)
    } else { # no time series, output is constant
      for (var in names(aux))
        eq[[var]] <- aux[[var]] <- eval(aux[[var]], enclos = model$modelEnv)
      
      eqTrajectory <- as.data.frame(rbind(eqTrajectory, 
                                          c(time = from, unlist(eq))), 
                                    row.names = NULL)
    }
    
    # Assemble output object
    output <- sdOutputClass$new(
      outTrajectory = eqTrajectory,
      auxTrajectory = NULL,
      timeSeriesTrajectory = tsTrajectory,
      model = model,
      scenario = scenario,
      diagnostics = NULL,
      postProcessOut = NULL)
  } else { # at least one atomic component
    if (is.null(method)) { 
      warning(sprintf(sdSimulatorMsg$sdSimulateAtomic5,model$id))
      method <- "lsoda"
    }
    if (is.null(st) || length(st) == 0)
      stop(sprintf(sdSimulatorMsg$sdSimulateCoupled4,model$id))
    
    createCoupledFuncEval <- CreateCoupledFuncEval
    
    environment(createCoupledFuncEval) <- model$modelEnv
    
    # get the aux connections index
    conAux <- match(unlist(model$eqConnections, use.names = F), names(aux))
    conAuxInps <- match(names(model$eqConnections), names(inp))
    
    # get the connected st index and the connected inp index
    conSt <- match(unlist(model$stConnections, use.names = F), names(st))
    conStInps <- match(names(model$stConnections), names(inp))
    
    # check if the match outputed any NA values
    compIndex <- model$indexComponents
    odeCoupledEval <- createCoupledFuncEval(
      componentsId = names(componentsEquations),
      funcs = componentsEquations,
      conSt = conSt,
      conStInps = conStInps,
      conAux = conAux,
      conAuxInps = conAuxInps,
      compIndex = compIndex,
      lenst = length(st),
      ct = ct,
      par = par,
      inp = inp,
      sw = sw,
      aux = aux,
      storeAuxTrajectory = storeAuxTrajectory)
    
    times <- seq(from = from,
                 to = to,
                 by = by)
    
    # Run simulation without root function
    if (!events || is.null(componentsTrigger) ||
        length(componentsTrigger) == 0) { 
      outTrajectory <- deSolve::ode(
        y = unlist(st),
        times = times,
        func = odeCoupledEval,
        parms = NULL,
        method = method
      )
    } else { # Run simulation with root 
      # check if the method support events
      if (!identical(method, deSolve::radau) &&
          !identical(method, deSolve::lsoda) &&
          !identical(method, deSolve::lsode) &&
          (!is.vector(method) ||
           !method %in% c("lsoda", "lsode", "radau"))) { 
        warning(sprintf(sdSimulatorMsg$sdSimulateCoupled5,model$id))
        
        method <- "lsoda"
      }
      
      createCoupledRootEventFunc <- CreateCoupledRootEventFunc
      environment(createCoupledRootEventFunc) <-  environment(
        createCoupledFuncEval)
      RootEventFuncsEval <- createCoupledRootEventFunc(
        componentsId = names(componentsTrigger),
        rootFuncs = componentsTrigger,
        eventFuncs = componentsEvent,
        conSt = conSt,
        conStInps = conStInps,
        conAux = conAux,
        conAuxInps = conAuxInps,
        compIndex = compIndex,
        ct = ct,
        par = par,
        inp = inp,
        sw = sw,
        auxiliary = aux)
      
      # Run simulation without event, stop in 1st root
      if (is.null(componentsEvent) ||
          length(componentsEvent) == 0) { 
        outTrajectory <- deSolve::ode(
          y = unlist(st),
          times = times,
          func = odeCoupledEval,
          parms = NULL,
          rootfunc = RootEventFuncsEval$coupledRootFunc,
          events = list(func = NULL,
                        root = T,
                        maxroots = maxroots),
          method = method)
      } else { # Run simulation with event
        outTrajectory <- deSolve::ode(
          y = unlist(st),
          times = times,
          func = odeCoupledEval,
          parms = NULL,
          rootfunc = RootEventFuncsEval$coupledRootFunc,
          events = list(func = RootEventFuncsEval$coupledEventFunc,
                        root = T,
                        maxroots = maxroots),
          method = method)
      }
    }
    
    diagnostics <- paste(capture.output(deSolve::diagnostics(outTrajectory)), 
                         collapse = "\n")
    
    # Calculate input trajectory
    if (storeTimeSeriesTrajectory && length(inp$fun_) > 0) { 
      tsTrajectory <- data.frame(time = outTrajectory[, "time"])
      
      for (x in inp$fun_)
        tsTrajectory <- cbind(tsTrajectory, x(tsTrajectory[, "time"]))
      
      colnames(tsTrajectory) <- c("time", names(inp$fun_))
    } else {
      tsTrajectory <- NULL
    }
    
    if (storeAuxTrajectory && length(aux) > 0) { 
      # separate the aux trajectory from the output trajectory
      iAuxBegin <- 2 + length(st)
      auxTrajectory <-
        as.data.frame(outTrajectory[, c(1, iAuxBegin:ncol(outTrajectory))])
      
      # separate the algebraic equations
      staticComponents <- names(
        which(model$componentsClass == sdStaticModelClass$classname))
      
      outTrajectory <-
        as.data.frame(outTrajectory[, c(1:(iAuxBegin - 1), 
                                        unlist(compIndex$aux[staticComponents]) + iAuxBegin - 2 +
                                          length(auxTrajectory) - length(aux))])
      
      auxTrajectory[unlist(compIndex$aux[staticComponents]) + 
                      length(auxTrajectory) - length(aux)] <- NULL
    } else { 
      outTrajectory <- as.data.frame(outTrajectory)
      auxTrajectory <- NULL
    }
    
    # run post process func
    if (!is.null(componentsPostProcessVars)) { 
      postProcess <- list()
      for (modelId in names(componentsPostProcessVars))
        postProcess[[modelId]] <- tryCatch(
          componentsPostProcessVars[[modelId]](outTrajectory,
                                               auxTrajectory,
                                               tsTrajectory,
                                               ct,
                                               par,
                                               inp,
                                               sw),
          error = function(e) {
             warning(sprintf(sdSimulatorMsg$sdSimulateCoupled6,
                             model$id, modelId, e))
            return(NULL)
          })
    } else {
      postProcess <- NULL
    }
    
    # Assemble output object
    output <- sdOutputClass$new(
      outTrajectory = outTrajectory,
      auxTrajectory = auxTrajectory,
      timeSeriesTrajectory = tsTrajectory,
      model = model,
      scenario = scenario,
      diagnostics = diagnostics,
      postProcessOut = postProcess)
  }
  return(output)
}
