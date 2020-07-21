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
      
      if(!is.null(parms)) # set any value in LSODA4R.cpp
        unlistReturn <- TRUE
      
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
                                  unlistReturn = F,
                                  storeAuxTrajectory = F,
                                  stNames = NULL) { 
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
    if(!is.null(stNames))
      names(st) <- stNames

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
    
    if(!is.null(parms)) # set any value in LSODA4R.cpp
      unlistReturn <- TRUE
    
    # Save aux trajectory
    if (storeAuxTrajectory)
      if(unlistReturn)
        return(unlist(c(dState, dAux, aux)))
      else
        return(list(dState, dAux, aux))
    else {
      if(unlistReturn)
        return(unlist(c(dState, dAux)))
      else
        return(list(dState, dAux))
    }
      
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
        stop(sprintf(sdSimulatorMsg$initialize1))
      private$pModel <- model
      
      #coupled Model verifications
      if (inherits(model, sdCoupledModelClass$classname)) {
        scenario  <- buildModel(model = model, scenario = scenario, 
                                from = from, to = to, by = by, 
                                method = method)
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
            warning(sprintf(sdSimulatorMsg$initialize4,
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
          stop(sprintf(sdSimulatorMsg$initialize5, model$id), 
               call. = FALSE)
      } else {
        stop(sprintf(sdSimulatorMsg$initialize5, model$id), 
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
        stop(sprintf(sdSimulatorMsg$initialize6,private$pModel$id))
      else if (!(abs(private$pTimes$to - private$pTimes$from) >= abs(private$pTimes$by) && 
                 (private$pTimes$to - private$pTimes$from)*private$pTimes$by > 0)) # invalid time sequence
        stop(sprintf(sdSimulatorMsg$initialize7,private$pModel$id))
      
      
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
      cat(indent("$output", indent = 4), sep = "\n")
      cat(indent(capture.output(private$pOutput), indent = 4), sep = "\n")
    },
    initModel = function(verbose = F) {
      
      if (!private$pModel$isVerified)
        private$pModel$verifyModel(private$pSimScenario, verbose = verbose)
      
      if (inherits(private$pModel, sdOdeModelClass$classname)) {
        out <- initOdeModel(private$pModel, private$pSimScenario)
      }
      else if (inherits(private$pModel, sdStaticModelClass$classname)) {
        out <- initStaticModel(private$pModel, private$pSimScenario)
      }
      else if (inherits(private$pModel, sdCoupledModelClass$classname)) {
        out <- initCoupledModel(private$pModel, private$pSimScenario)
      }
      
      private$pOdeEnv <- out$odeEnv
      private$pOde <- out$ode
      
      private$pCurrTime <- private$pTimes$from
      private$pCurrState <- private$pSimScenario$state
      
      private$pOutput <- sdOutputClass$new(
        outTrajectory = c(private$pTimes$from, unlist(private$pSimScenario$state, use.names = F)),
        auxTrajectory = NULL,
        timeSeriesTrajectory = NULL,
        model = private$pModel,
        scenario = private$pSimScenario,
        diagnostics = NULL,
        postProcessOut = NULL)

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
        output <- runOdeSimulation(private$pOdeEnv, private$pOde, private$pModel, private$pTimes$from, private$pTimes$to, 
                         private$pTimes$by, private$pMethod, events, maxroots, terminalroot,
                         ties, storeAuxTrajectory, storeTimeSeriesTrajectory)
      } 
      #if the model is static
      else if (inherits(private$pModel, sdStaticModelClass$classname)) {
        output <- runStaticSimulation(private$pOdeEnv, private$pModel, private$pTimes$from, private$pTimes$to, 
                            private$pTimes$by, private$pMethod, storeTimeSeriesTrajectory, verbose)
      } 
      #if the model is coupled
      else if (inherits(private$pModel, sdCoupledModelClass$classname)) {
        output <- runCoupledSimulation(private$pOdeEnv, private$pOde, private$pModel, private$pTimes$from, private$pTimes$to, 
                             private$pTimes$by, private$pMethod, events, maxroots,
                             storeAuxTrajectory, storeTimeSeriesTrajectory, verbose)
      } else {
        stop(sprintf(sdSimulatorMsg$runSimulation1))
      }
      
      private$pOutput$setOutTraj(output$outTrajectory)
      private$pOutput$setAuxTraj(output$auxTrajectory)
      private$pOutput$setTimeSeries(output$timeSeriesTrajectory)
      private$pOutput$setPostProcess(output$diagnostics)
      private$pOutput$setDiagnostics(output$postProcessOut)
    },
    runStep = function(from = NULL, to = NULL, by = NULL) {
      if(is.null(private$pOde)) {
        warning(sprintf(sdSimulatorMsg$runStep3, private$pModel$id))
        return()
      }
        
      if (is.null(from))
        from <- private$pCurrTime

      if(private$pCurrTime > from) {
        warning(sprintf(sdSimulatorMsg$runStep1, private$pCurrTime))
        from <- private$pCurrTime
      }
      
      if (is.null(by))
        by <- private$pTimes$by
      if (is.null(to))
        to <- from + by

      if(from > to) {
        stop(sprintf(sdSimulatorMsg$runStep2))
      }

      if(to - from - by < 1e-6) { # If one step only
        out <- sdsim::runStep(private$pOde, c(from, to), private$pCurrState)
        currState <- out$state[-1]

      } else { # More than one step
        times <- seq(from, to, by)
        out <- sdsim::runStep(private$pOde, times, private$pCurrState)

        # Get last state
        currState <- tail(out$state, length(private$pCurrState))
      }

      # Save trajectory and update current state and time
      private$pOutput$updateOutTraj(out$state)
      private$pCurrState <- setNames(as.list(currState), names(private$pCurrState))
      private$pCurrTime <- to
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
    },
    currentTime = function() {
      return(private$pCurrTime)
    },
    currentState = function() {
      return(private$pCurrState)
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

runOdeSimulation <- function(env, ode,
                             model,
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
  
  st <- env$st
  ct <- env$ct
  par <- env$par
  inp <- env$inp
  sw <- env$sw

  odeEval <- ode

  
  # Run simulation without root function, data frame or times vector
  if (!events || is.null(trigger) ||
      (!is.function(trigger) &&
       !is.data.frame(trigger) && 
       !is.numeric(trigger))) { 
    # Run simulation without support to events
    outTrajectory <- deSolve::ode(
      y = unlist(st),
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
        warning(sprintf(sdSimulatorMsg$runSimulationAtomic1,model$id))
        method <- "lsoda"
      }
      
      triggerEval <-
        CreateFuncEval(func = trigger, env = env, auxiliary = auxiliary, 
                       lastEvalTime = (from - 1))
      
      if (is.function(event)) { 
        
        # EVENTS func triggered by a root function
        eventEval <- CreateFuncEval(event,
                                            env = env, 
                                            auxiliary = auxiliary,
                                            lastEvalTime = (from - 1),
                                            unlistReturn = T)
        
        outTrajectory <- deSolve::ode(
          y = unlist(st),
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
          y = unlist(st),
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
        warning(sprintf(sdSimulatorMsg$runSimulationAtomic1,model$id))
        method <- "lsoda"
      }
      # events in a function with the triggers times in trigger
      eventEval <-
        CreateFuncEval(event,
                       env,
                       auxiliary = auxiliary,
                       lastEvalTime = (from - 1),
                       unlistReturn = T)
      
      outTrajectory <- deSolve::ode(
        y = unlist(st),
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
        warning(sprintf(sdSimulatorMsg$runSimulationAtomic1,model$id))
        method <- "lsoda"
      }
      outTrajectory <- deSolve::ode(
        y = unlist(st),
        times = seq(from, to, by),
        func = odeEval,
        parms = NULL,
        events = list(data = trigger,
                      ties = ties),
        method = method)
    } else { # run without events / maybe no event func for the time vector
      outTrajectory <- deSolve::ode(
        y = unlist(st),
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
    iAuxBegin <- 2 + length(st)
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
        warning(sprintf(sdSimulatorMsg$runSimulationAtomic2, model$id, e))
        return(NULL)
      })
  
  # Assemble output
  output <- list(
    outTrajectory = outTrajectory,
    auxTrajectory = auxTrajectory,
    timeSeriesTrajectory = tsTrajectory,
    diagnostics = diagnostics,
    postProcessOut = postProcess)
  
  return(output)
}

runStaticSimulation <- function(env, model,
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
    stop(sprintf(sdSimulatorMsg$runSimulationStatic1,model$id))
  
  ct <- env$ct
  par <- env$par
  inp <- env$inp
  sw <- env$sw

  if (length(globalfuns) > 0)
    auxenv <- environment(globalfuns[[1]])
  else
    auxenv <- environment()
  
  eqTrajectory <- c()
  tsTrajectory <- NULL
  eq <- list()
  if (length(inp$fun_) > 0) { 
    timeSeries <- which(names(inp) %in% names(inp$fun_))
    for (t in seq(from, to, by)) { 
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
                                        c(time = from, unlist(eq))), 
                                  row.names = NULL)
  }
  
  # Assemble output object
  output <- list(
    outTrajectory = eqTrajectory,
    auxTrajectory = NULL,
    timeSeriesTrajectory = tsTrajectory,
    diagnostics = NULL,
    postProcessOut = NULL)

  return(output)
}

runCoupledSimulation <- function(env, ode,
                                 model,
                                 from = NULL,
                                 to = NULL,
                                 by = NULL,
                                 method = NULL,
                                 events = TRUE,
                                 maxroots = 100,
                                 storeAuxTrajectory = T,
                                 storeTimeSeriesTrajectory = F,
                                 verbose = F){

  # Get model functions
  componentsEquations <- model$componentsEquations
  componentsPostProcessVars <- model$componentsPostProcessVars
  componentsTrigger <-
    model$componentsTrigger
  componentsEvent <- model$componentsEvent
  aux <- model$componentsAux
  componentsId <- model$componentsId
  
  # get model variables
  st <- env$st
  ct <- env$ct
  par <- env$par
  inp <- env$inp
  sw <- env$sw
  
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
    output <- list(
      outTrajectory = eqTrajectory,
      auxTrajectory = NULL,
      timeSeriesTrajectory = tsTrajectory,
      diagnostics = NULL,
      postProcessOut = NULL)
  } else { # at least one atomic component
    if (is.null(method)) { 
      warning(sprintf(sdSimulatorMsg$initialize8,model$id))
      method <- "lsoda"
    }
    if (is.null(st) || length(st) == 0)
      stop(sprintf(sdSimulatorMsg$runSimulationCoupled1,model$id))
    
    # get the aux connections index
    conAux <- match(unlist(model$eqConnections, use.names = F), names(aux))
    conAuxInps <- match(names(model$eqConnections), names(inp))
    
    # get the connected st index and the connected inp index
    conSt <- match(unlist(model$stConnections, use.names = F), names(st))
    conStInps <- match(names(model$stConnections), names(inp))
    
    # check if the match outputed any NA values
    compIndex <- model$indexComponents
    odeCoupledEval <- ode
    
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
        warning(sprintf(sdSimulatorMsg$runSimulationCoupled2,model$id))
        
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
             warning(sprintf(sdSimulatorMsg$runSimulationCoupled3,
                             model$id, modelId, e))
            return(NULL)
          })
    } else {
      postProcess <- NULL
    }
    
    # Assemble output object
    output <- list(
      outTrajectory = outTrajectory,
      auxTrajectory = auxTrajectory,
      timeSeriesTrajectory = tsTrajectory,
      diagnostics = diagnostics,
      postProcessOut = postProcess)
  }
  return(output)
}

buildModel <- function(model, scenario, from = NULL, to = NULL, by = NULL, method = NULL) {
  # Try to build the coupled model if it is not built
  if (!model$isBuilt) { 
    model$buildCoupledModel(from = from,
                            to = to,
                            by = by,
                            method = method)
    
    if (!model$isBuilt)
      stop(sprintf(sdSimulatorMsg$initialize2,model$id))
  }
  
  if (length(model$componentsEquations) == 0 && 
      length(model$componentsAux) == 0)
    stop(sprintf(sdSimulatorMsg$initialize3,model$id))
  
  # convert list of scenarios to coupled scenario
  if (is.list(scenario))
    scenario <- sdBuildCoupledScenario(id = "coupledScen", 
                                       scenarios = scenario)
  return(scenario)
}

initOdeModel <- function(model, scenario) {
  
  # stop if model is empty
  if (is.null(model$ode))
    stop(sprintf(sdSimulatorMsg$initAtomic1, model$id), call. = FALSE)

  odeEnv <- new.env(parent = emptyenv())

  initVars <- model$initVars

  # Get variables from default scenario
  st <- scenario$state
  ct <- scenario$constant
  par <- scenario$parameter
  inp <- scenario$input
  sw <- scenario$switch

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

  assign("st", st, odeEnv)
  assign("ct", ct, odeEnv)
  assign("par", par, odeEnv)
  assign("inp", inp, odeEnv)
  assign("sw", sw, odeEnv)

  # verify state variables
  if (is.null(st) || length(st) == 0)
    stop(sprintf(sdSimulatorMsg$initAtomic2,model$id))

  environment(CreateFuncEval) <- model$modelEnvironment

  ode <-
    CreateFuncEval(func = model$ode,
                   odeEnv,
                   auxiliary = model$aux,
                   lastEvalTime = (model$defaultScenario$times$from - 1), # TODO: mudar para nulo?
                   storeAuxTrajectory = T,
                   unlistReturn = F,
                   stNames = names(st))
  return(list(odeEnv = odeEnv, ode = ode))
}

initStaticModel <- function(model, scenario) {
  
  odeEnv <- new.env(parent = emptyenv())
  
  initVars <- model$initVars
  
  # Get variables from default scenario
  ct <- scenario$constant
  par <- scenario$parameter
  inp <- scenario$input
  sw <- scenario$switch
  
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
  
  assign("ct", ct, odeEnv)
  assign("par", par, odeEnv)
  assign("inp", inp, odeEnv)
  assign("sw", sw, odeEnv)
  
  return(list(odeEnv = odeEnv, ode = NULL))
}

initCoupledModel <- function(model, scenario) {
  
  odeEnv <- new.env(parent = emptyenv())
  
  componentsInitVars <- model$componentsInitVars
  componentsId <- model$componentsId
  
  # Get variables from default scenario
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

  assign("st", st, odeEnv)
  assign("ct", ct, odeEnv)
  assign("par", par, odeEnv)
  assign("inp", inp, odeEnv)
  assign("sw", sw, odeEnv)
  
  componentsEquations <- model$componentsEquations
  
  if (length(componentsEquations) > 0) {
    
    createCoupledFuncEval <- CreateCoupledFuncEval
    
    environment(createCoupledFuncEval) <- model$modelEnv
    
    # get the aux connections index
    conAux <- match(unlist(model$eqConnections, use.names = F), names(model$componentsAux))
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
      aux = model$componentsAux,
      unlistReturn = F,
      storeAuxTrajectory = T,
      stNames = names(st))
    
  }

  return(list(odeEnv = odeEnv, ode = odeCoupledEval))
}
