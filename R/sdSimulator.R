CreateFuncEval <-
  function(func,
           ct,
           par,
           inp,
           sw,
           auxiliary,
           lastEvalTime,
           unlistReturn = F,
           storeAuxTrajectory = F)
  {
    aux <- vector("list", length = length(auxiliary))
    names(aux) <- names(auxiliary)
    auxseq <- seq_along(auxiliary)
    timeSeries <- match(names(inp$fun_), names(inp))
    
    FuncEval <- function(t, st, parms)
    {
      st <- as.list(st)
      
      # evaluate time series and auxiliary varibles if the last evaluation time
      # is different than the current time
      if (lastEvalTime != t)
      {
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
                                  storeAuxTrajectory = F) 
{
  lastEvalTime <- -Inf
  
  modelseq <- seq_along(componentsId)
  
  aux <- eq <- vector("list", length = length(auxiliary))
  names(aux) <- names(eq) <- names(auxiliary)
  auxseq <- seq_along(auxiliary)
  timeSeries <- match(names(inp$fun_), names(inp))
  
  dState <- vector("numeric", length = lenst)
  dAux <- list()
  
  CoupledFuncEval <- function(t, state, parms)
  {
    # stores the model definitions result
    st <- as.list(state)
    
    # evaluate time series varibles if the last evaluation time
    # is different than the current time
    if (lastEvalTime != t)
    {
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
    for (i in modelseq)
    {
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
      
      if (length(mAux) > 0)
        # there is auxiliary values
      {
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
           auxiliary)
  {
    triggedEvents <- c()  # stores which models trigged events
    lastEvalTime <- -Inf
    
    aux <- eq <- vector("list", length = length(auxiliary))
    names(aux) <- names(eq) <- names(auxiliary)
    auxseq <- seq_along(auxiliary)
    
    timeSeries <- match(names(inp$fun_), names(inp))
    
    CoupledRootEval <- function(t, state, parms)
    {
      triggedEvents <<- c()
      modelRoots <- roots <- c() # the roots returned by all models
      st <- as.list(state)
      
      # evaluate time series varibles if the last evaluation time
      # is different than the current time
      if (lastEvalTime != t)
      {
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
      for (modelId in componentsId)
      {
        if (is.function(rootFuncs[[modelId]]))
          modelRoots <- rootFuncs[[modelId]](
            t = t,
            st = st[compIndex[["st"]][[modelId]]],
            ct = ct[compIndex[["ct"]][[modelId]]],
            par = par[compIndex[["par"]][[modelId]]],
            inp = inp[compIndex[["inp"]][[modelId]]],
            sw = sw[compIndex[["sw"]][[modelId]]],
            aux = aux[compIndex[["aux"]][[modelId]]])
        else if (is.data.frame(rootFuncs[[modelId]]))
        {
          if (any(rootFuncs[[modelId]]$time == t))
            modelRoots <- 0
          else 
            modelRoots <- 1
        }
        else if (is.numeric(rootFuncs[[modelId]]))
        {
          modelRoots <- rootFuncs[[modelId]] - t
        }
          
        roots <- c(roots, modelRoots)
        
        # save the models that trigged an event
        if (any(modelRoots == 0))
          triggedEvents <<- c(triggedEvents, modelId)
      }
      
      return(roots)
    }
    
    CoupledEventEval <- function(t, state, parms)
    {
      st <- as.list(state)
      
      if (!is.null(triggedEvents) || length(triggedEvents) > 0)
      {
        triggedEvents <<- unique(triggedEvents)
        
        for (modelId in triggedEvents)
        {
          if (!is.null(eventFuncs[[modelId]]))
          {
            # run event
            if (is.function(eventFuncs[[modelId]]))
            {
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
            }
            else if (is.data.frame(eventFuncs[[modelId]]))
            {
              trigged <- eventFuncs[[modelId]][eventFuncs[[modelId]]$time == t,]
              
              for (i in 1:nrow(trigged))
              {
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
#' If performance is crucial remember to garantee that the model is already 
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
sdSimulate <- function(model,
                       scenario = NULL,
                       from = NULL,
                       to = NULL,
                       by = NULL,
                       method = NULL,
                       events = T,
                       maxroots = 100,
                       terminalroot = NULL,
                       ties = "notordered",
                       storeAuxTrajectory = T,
                       storeTimeSeriesTrajectory = F,
                       verbose = F)
{
  if (missing(model))
    sdSimulatorMsg$sdSimulate()
  
  # Save method argument
  methodArg <- method
  
  # If the model is atomic
  if (inherits(model, sdOdeModelClass$classname))
  {
    # stop if model is empty
    if (is.null(model$DifferentialEquations))
      stop(sprintf(sdSimulatorMsg$sdSimulateAtomic7, model$id), call. = FALSE)
    
    if (!model$isVerified)
      model$verifyModel(scenario, verbose = verbose)
    
    # Get model functions
    InitVars <- model$InitVars
    PostProcessVars <- model$PostProcessVars
    RootSpecification <- model$RootSpecification
    EventFunction <- model$EventFunction
    auxiliary <- model$aux
    
    # get the simulation scenario
    if (!is.null(model$defaultScenario))
    {
      # get the model scenario 
      defaultScenario <- model$defaultScenario$clone(deep = TRUE)
    
      # overwrite default variables with the given scenario values
      if (!is.null(scenario))
      {
        if (is.character(scenario))
          scenario <- sdLoadScenario(file = scenario)
        
        if (inherits(scenario, sdScenarioClass$classname))
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
          if (!is.null(scenario$method))
            defaultScenario$method <- scenario$method
        }
        else
          sdSimulatorMsg$sdSimulateAtomic6(model$id, typeof(scenario))
      }
    }
    else if (!is.null(scenario))
    {
      if (is.character(scenario))
        scenario <- sdLoadScenario(file = scenario)
      
      if (inherits(scenario, sdScenarioClass$classname))
        defaultScenario <- scenario
      else
        stop(sprintf(sdSimulatorMsg$sdSimulateModel, "Atomic", model$id), 
             call. = FALSE)
    }
    else
      stop(sprintf(sdSimulatorMsg$sdSimulateModel, "Atomic", model$id), 
           call. = FALSE)

    # Get variables from default scenario
    state <- defaultScenario$state
    ct <- defaultScenario$constant
    par <- defaultScenario$parameter
    inp <- defaultScenario$input
    sw <- defaultScenario$switch
    times <- defaultScenario$times
    method <- defaultScenario$method
    
    # Override scenario times and method with the function parameters
    if (!is.null(from))
      times$from <- from
    if (!is.null(to))
      times$to <- to
    if (!is.null(by))
      times$by <- by
    if (!is.null(methodArg))
      method <- methodArg
    
    # verify data
    if (is.null(times) || !all(c("from", "to", "by") %in% names(times)))
      sdSimulatorMsg$sdSimulateAtomic2(model$id)
    if (is.null(method))
    {
      sdSimulatorMsg$sdSimulateAtomic5(model$id)
      method <- "lsoda"
    }
      
    # run the InitVars function
    if (!is.null(InitVars))
    {
      modelInit <- InitVars(st = state,
                           ct = ct,
                           par = par,
                           inp = inp,
                           sw = sw,
                           aux = auxiliary)
      state <- modelInit$st
      ct <- modelInit$ct
      par <- modelInit$par
      inp <- modelInit$inp
      sw <- modelInit$sw
    }
    
    # verify state variables
    if (is.null(state) || length(state) == 0)
      sdSimulatorMsg$sdSimulateAtomic1(model$id)
    
    createFuncEval <- CreateFuncEval
    environment(createFuncEval) <- environment(model$DifferentialEquations)
    DifferentialEquationsEval <-
      createFuncEval(func = model$DifferentialEquations,
                     ct = ct,
                     par = par,
                     inp = inp,
                     sw = sw,
                     auxiliary = auxiliary,
                     lastEvalTime = (times$from - 1),
                     storeAuxTrajectory = storeAuxTrajectory)
    
    # Run simulation without root function, data frame or times vector
    if (!events || is.null(RootSpecification) ||
        (!is.function(RootSpecification) &&
          !is.data.frame(RootSpecification)
          && !is.numeric(RootSpecification)))
    {
      # Run simulation without support to events
      outTrajectory <- deSolve::ode(
        y = unlist(state),
        times = seq(times$from, times$to, times$by),
        parms = NULL,
        func = DifferentialEquationsEval,
        method = method
      )
    }
    else
      # Run simulation with support to events
    {
      if (is.function(RootSpecification))
      {
        # check if the method support events
        if (!identical(method, deSolve::radau) &&
            !identical(method, deSolve::lsoda) &&
            !identical(method, deSolve::lsode) &&
            (!is.vector(method) ||
             !method %in% c("lsoda", "lsode", "radau")))
        {
          sdSimulatorMsg$sdSimulateAtomic3(model$id)
          method <- "lsoda"
        }
        RootSpecificationEval <-
          createFuncEval(func = RootSpecification, ct = ct, par = par, 
                         inp = inp, sw = sw, auxiliary = auxiliary, 
                         lastEvalTime = (times$from - 1))
        
        if (is.function(EventFunction))
        {
          # EVENTS func triggered by a root function
          EventFunctionEval <- createFuncEval(EventFunction,
                                              ct, par, inp, sw, 
                                              auxiliary = auxiliary,
                                              lastEvalTime = (times$from - 1),
                                              unlistReturn = T)
          
          outTrajectory <- deSolve::ode(
            y = unlist(state),
            times = seq(times$from, times$to, times$by),
            func = DifferentialEquationsEval,
            parms = NULL,
            rootfunc = RootSpecificationEval,
            events = list(
              func = EventFunctionEval,
              root = T,
              maxroots = maxroots,
              terminalroot = terminalroot),
            method = method
          )
        }
        else
          # no event func, stop in the first root func
          outTrajectory <- deSolve::ode(
            y = unlist(state),
            times = seq(times$from, times$to, times$by),
            func = DifferentialEquationsEval,
            parms = NULL,
            rootfunc = RootSpecificationEval,
            events = list(
              root = T,
              maxroots = maxroots,
              terminalroot = terminalroot
            ),
            method = method
          )
      }
      else if (is.numeric(RootSpecification) &&
               is.function(EventFunction))
      {
        # events in a function with the triggers times in RootSpecification
        EventFunctionEval <-
          createFuncEval(EventFunction,
                         ct,
                         par,
                         inp,
                         sw,
                         auxiliary = auxiliary,
                         lastEvalTime = (times$from - 1),
                         unlistReturn = T)
        
        outTrajectory <- deSolve::ode(
          y = unlist(state),
          times = seq(times$from, times$to, times$by),
          func = DifferentialEquationsEval,
          parms = NULL,
          events = list(func = EventFunctionEval,
                        time = RootSpecification),
          method = method)
      }
      else if (is.data.frame(RootSpecification))
      {
        outTrajectory <- deSolve::ode(
          y = unlist(state),
          times = seq(times$from, times$to, times$by),
          func = DifferentialEquationsEval,
          parms = NULL,
          events = list(data = RootSpecification,
                        ties = ties),
          method = method)
      }
      else
        # run without events / maybe no event func for the time vector
      {
        outTrajectory <- deSolve::ode(
          y = unlist(state),
          times = seq(times$from, times$to, times$by),
          parms = NULL,
          func = DifferentialEquationsEval,
          method = method
        )
      }
    }
    
    diagnostics <-
      paste(capture.output(deSolve::diagnostics(outTrajectory)), 
            collapse = "\n")
    
    # Calculate time series trajectory
    if (storeTimeSeriesTrajectory && length(inp$interpolation_) > 0)
    {
      tsTrajectory <- data.frame(time = outTrajectory[, "time"])
      
      for (x in inp$fun_)
        tsTrajectory <-
          cbind(tsTrajectory, as.numeric(x(tsTrajectory[, "time"])))
      
      colnames(tsTrajectory) <- c("time", names(inp$fun_))
    }
    else
    {
      tsTrajectory <- NULL
    }
    
    if (storeAuxTrajectory && length(auxiliary) > 0)
    {
      # separate the aux trajectory from the output trajectory
      # Auxiliaries begin at time column + length(state) + 1
      iAuxBegin <- 2 + length(state)
      outTrajectory <- as.data.frame(outTrajectory)
      auxTrajectory <- outTrajectory[, c(1, iAuxBegin:ncol(outTrajectory))]
      outTrajectory <- outTrajectory[, 1:(iAuxBegin - 1)]
    }
    else
    {
      outTrajectory <- as.data.frame(outTrajectory)
      auxTrajectory <- NULL
    }
    
    # run post process func
    postProcess <- NULL
    if (!is.null(PostProcessVars))
      postProcess <- tryCatch(
        PostProcessVars(outTrajectory,
                        auxTrajectory, tsTrajectory,
                        ct, par, inp, sw),
        error = function(e)
        {
          sdSimulatorMsg$sdSimulateAtomic4(model$id, e)
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
      postProcessValue = postProcess)
    
    return(output)
  }
  else if (inherits(model, sdStaticModelClass$classname))
  {
    # Get model attributes
    equations <- model$algebraicEquations
    InitVars <- model$InitVars
    globalfuns <- model$GlobalFunctions
    
    # stop if model is empty
    if (length(equations) == 0)
      sdSimulatorMsg$sdSimulateStatic0(model$id)
    
    if (!model$isVerified)
      model$verifyModel(scenario, verbose = verbose)
    
    
    # get the simulation scenario
    if (!is.null(model$defaultScenario))
    {
      # get the model scenario 
      defaultScenario <- model$defaultScenario$clone(deep = TRUE)
      
      # overwrite default variables with the given scenario values
      if (!is.null(scenario))
      {
        if (is.character(scenario))
          scenario <- sdLoadScenario(file = scenario)
        
        if (inherits(scenario, sdScenarioClass$classname))
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
        }
        else
          sdSimulatorMsg$sdSimulateAtomic6(model$id, 
                                           typeof(scenario))
      }
    }
    else if (!is.null(scenario))
    {
      if (is.character(scenario))
        scenario <- sdLoadScenario(file = scenario)
      
      if (inherits(scenario, sdScenarioClass$classname))
        defaultScenario <- scenario
      else
        stop(sprintf(sdSimulatorMsg$sdSimulateModel, "Static", model$id), 
             call. = FALSE)
    }
    else
      stop(sprintf(sdSimulatorMsg$sdSimulateModel, "Static", model$id), 
           call. = FALSE)
    
    # Get variables from default scenario
    ct <- defaultScenario$constant
    par <- defaultScenario$parameter
    inp <- defaultScenario$input
    sw <- defaultScenario$switch
    times <- defaultScenario$times
    
    # Override scenario times and method with the function parameters
    if (!is.null(from))
      times$from <- from
    if (!is.null(to))
      times$to <- to
    if (!is.null(by))
      times$by <- by
    
    # verify data
    if (is.null(times) || !all(c("from", "to", "by") %in% names(times)))
      sdSimulatorMsg$sdSimulateStatic1(model$id)
    
    if (!is.null(InitVars))
    {
      modelInit <-
        InitVars(ct = ct,
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
    if (length(inp$fun_) > 0)
    {
      timeSeries <- which(names(inp) %in% names(inp$fun_))
      for (t in seq(times$from, times$to, times$by))
      {
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
    }
    else # no time series, output is constant
    {
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
      postProcessValue = NULL)
    
    return(output)
  }
  else if (inherits(model, sdCoupledModelClass$classname))
  {
    # Try to build the coupled model if it is not built
    if (!model$isBuilt)
    {
      model$buildCoupledModel(from = from,
                              to = to,
                              by = by,
                              method = method)
      
      if (!model$isBuilt)
        sdSimulatorMsg$sdSimulateCoupled1(model$id)
    }
    
    if (length(model$componentsEquations) == 0 && 
        length(model$componentsAux) == 0)
      sdSimulatorMsg$sdSimulateCoupled0(model$id)
    
    # convert list of scenarios to coupled scenario
    if (is.list(scenario))
      scenario <- sdBuildCoupledScenario(id = "coupledScen", 
                                         scenarios = scenario)
    
    if (!model$isVerified)
      model$verifyModel(scenario = scenario, verbose = verbose)
    
    # Get model functions
    componentsEquations <- model$componentsEquations
    componentsInitVars <- model$componentsInitVars
    componentsPostProcessVars <- model$componentsPostProcessVars
    componentsRootSpecification <-
      model$componentsRootSpecification
    componentsEventFunction <- model$componentsEventFunction
    aux <- model$componentsAux
    componentsId <- model$componentsId
    
    # get the simulation scenario
    if (!is.null(model$defaultScenario))
    {
      # get the model scenario 
      defaultScenario <- model$defaultScenario$clone(deep = TRUE)
      
      # overwrite default variables with the given scenario values
      if (!is.null(scenario))
      {
        if (is.character(scenario))
          scenario <- sdLoadScenario(file = scenario)
        
        if (inherits(scenario, sdScenarioClass$classname))
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
          if (!is.null(scenario$method))
            defaultScenario$method <- scenario$method
        }
        else
          sdSimulatorMsg$sdSimulateAtomic6(model$id, 
                                           typeof(scenario))
      }
    }
    else if (!is.null(scenario))
    {
      if (is.character(scenario))
        scenario <- sdLoadScenario(file = scenario)
      
      if (inherits(scenario, sdScenarioClass$classname))
        defaultScenario <- scenario
      else
        stop(sprintf(sdSimulatorMsg$sdSimulateModel, "Coupled", model$id), 
             call. = FALSE)
    }
    else
      stop(sprintf(sdSimulatorMsg$sdSimulateModel, "Coupled", model$id), 
           call. = FALSE)
    
    # get model variables
    st <- defaultScenario$state
    ct <- defaultScenario$constant
    par <- defaultScenario$parameter
    inp <- defaultScenario$input
    sw <- defaultScenario$switch
    times <- defaultScenario$times
    method <- defaultScenario$method
    
    # Overwrite scenario times and method with the function parameters
    if (!is.null(from))
      times$from <- from
    if (!is.null(to))
      times$to <- to
    if (!is.null(by))
      times$by <- by
    if (!is.null(methodArg))
      method <- methodArg
    
    # verify time sequence
    if (is.null(times) || !all(c("from", "to", "by") %in% names(times)))
      sdSimulatorMsg$sdSimulateCoupled3(model$id)
    
    # Run the model Init Vars
    modelInit <- list()
    for (modelId in componentsId)
    {
      # run the init vars
      if (!is.null(componentsInitVars[[modelId]]))
      {
        if (inherits(model$components[[modelId]], sdOdeModelClass$classname))
          modelInitVars <-
            componentsInitVars[[modelId]](
              st = st[model$indexComponents$st[[modelId]]],
              ct = ct[model$indexComponents$ct[[modelId]]],
              par = par[model$indexComponents$par[[modelId]]],
              inp = inp[model$indexComponents$inp[[modelId]]],
              sw = sw[model$indexComponents$sw[[modelId]]],
              aux = aux[model$indexComponents$aux[[modelId]]])
        else if (inherits(model$components[[modelId]], 
                          sdStaticModelClass$classname))
          modelInitVars <-
            componentsInitVars[[modelId]](
              ct = ct[model$indexComponents$ct[[modelId]]],
              par = par[model$indexComponents$par[[modelId]]],
              inp = inp[model$indexComponents$inp[[modelId]]],
              sw = sw[model$indexComponents$sw[[modelId]]],
              eq = aux[model$indexComponents$aux[[modelId]]])
        else
          next()
        
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
    
    if (length(modelInit) > 0)
    {
      st  <- MergeLists(modelInit$st, st, "coupledState")
      ct  <- MergeLists(modelInit$ct, ct, "coupledConstant")
      par <- MergeLists(modelInit$par, par, "coupledParameter")
      inp <- MergeLists(modelInit$inp, inp, "coupledInput")
      sw  <- MergeLists(modelInit$sw, sw, "coupledSwitch")
    }
    
    # only static components
    if (length(componentsEquations) == 0)
    {
      eqTrajectory <- c()
      tsTrajectory <- NULL
      eq <- list()
      if (length(inp$fun_) > 0)
      {
        timeSeries <- which(names(inp) %in% names(inp$fun_))
        for (t in seq(times$from, times$to, times$by))
        {
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
      }
      else # no time series, output is constant
      {
        for (var in names(aux))
          eq[[var]] <- aux[[var]] <- eval(aux[[var]], enclos = model$modelEnv)
        
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
        postProcessValue = NULL)
    }
    else # at least one atomic component
    {
      if (is.null(method))
      {
        sdSimulatorMsg$sdSimulateAtomic5(model$id)
        method <- "lsoda"
      }
      if (is.null(st) || length(st) == 0)
        sdSimulatorMsg$sdSimulateCoupled4(model$id)
      
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
      DifferentialEquationsCoupledEval <- createCoupledFuncEval(
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
      
      times <- seq(from = times$from,
                   to = times$to,
                   by = times$by)
      
      # Run simulation without root function
      if (!events || is.null(componentsRootSpecification) ||
          length(componentsRootSpecification) == 0)
      {
        outTrajectory <- deSolve::ode(
          y = unlist(st),
          times = times,
          func = DifferentialEquationsCoupledEval,
          parms = NULL,
          method = method
        )
      }
      # Run simulation with root
      else
      {
        # check if the method support events
        if (!identical(method, deSolve::radau) &&
            !identical(method, deSolve::lsoda) &&
            !identical(method, deSolve::lsode) &&
            (!is.vector(method) ||
             !method %in% c("lsoda", "lsode", "radau")))
        {
          sdSimulatorMsg$sdSimulateCoupled5(model$id)
          method <- "lsoda"
        }
        
        createCoupledRootEventFunc <- CreateCoupledRootEventFunc
        environment(createCoupledRootEventFunc) <-  environment(
          createCoupledFuncEval)
        RootEventFuncsEval <- createCoupledRootEventFunc(
          componentsId = names(componentsRootSpecification),
          rootFuncs = componentsRootSpecification,
          eventFuncs = componentsEventFunction,
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
        if (is.null(componentsEventFunction) ||
            length(componentsEventFunction) == 0)
        {
          outTrajectory <- deSolve::ode(
            y = unlist(st),
            times = times,
            func = DifferentialEquationsCoupledEval,
            parms = NULL,
            rootfunc = RootEventFuncsEval$coupledRootFunc,
            events = list(func = NULL,
                          root = T,
                          maxroots = maxroots),
            method = method)
        }
        # Run simulation with event
        else
        {
          outTrajectory <- deSolve::ode(
            y = unlist(st),
            times = times,
            func = DifferentialEquationsCoupledEval,
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
      if (storeTimeSeriesTrajectory && length(inp$fun_) > 0)
      {
        tsTrajectory <- data.frame(time = outTrajectory[, "time"])
        
        for (x in inp$fun_)
          tsTrajectory <- cbind(tsTrajectory, x(tsTrajectory[, "time"]))
        
        colnames(tsTrajectory) <- c("time", names(inp$fun_))
      }
      else
        tsTrajectory <- NULL
      
      
      if (storeAuxTrajectory && length(aux) > 0)
      {
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
      }
      else
      {
        outTrajectory <- as.data.frame(outTrajectory)
        auxTrajectory <- NULL
      }
      
      # run post process func
      if (!is.null(componentsPostProcessVars))
      {
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
            error = function(e)
            {
              sdSimulatorMsg$sdSimulateCoupled6(model$id, modelId)
              return(NULL)
            })
      }
      else
        postProcess <- NULL
      
      # Assemble output object
      output <- sdOutputClass$new(
        outTrajectory = outTrajectory,
        auxTrajectory = auxTrajectory,
        timeSeriesTrajectory = tsTrajectory,
        model = model,
        scenario = scenario,
        diagnostics = diagnostics,
        postProcessValue = postProcess)
    }
    
    return(output)
  }
  else
    stop(sdSimulatorMsg$sdSimulate, call. = F)
}
