sdFlowOdeClass <- R6::R6Class(
  
  classname = "sdFlowOde",
  
  inherit = sdOdeClass,
  
  public = list(
    initialize = function(flows, flowRate, stocks, boundaries) { 
      private$pFlows <- flows
      
      # TODO: change message variables names
      if(is.null(flows))
        stop(sprintf(auxiliaryMsg$sdMakeFlows1))
      
      if(is.null(flowRate))
        stop(sprintf(auxiliaryMsg$sdMakeFlows2))
      
      if(length(flows) == 0)
        stop(sprintf(auxiliaryMsg$sdMakeFlows3))
      
      if(length(flowRate) == 0)
        stop(sprintf(auxiliaryMsg$sdMakeFlows4))
      
      if(is.list(flows))
        connections <- unlist(connections)
      
      if(is.list(flowRate))
        flowRate <- unlist(flowRate)
      
      if(length(flows) != length(flowRate))
        stop(sprintf(auxiliaryMsg$sdMakeFlows5))
      
      # Verifies if there are flows with inverted arrow, and corrects it
      flows <- private$verifyInverseFlow(flows)
      
      # Check if variables in flows are present in boundaries or st
      private$verifyBoundariesSt(boundaries, stocks, flows)
      
      split_flow <- strsplit(flows, split = "\\h*->\\h*", perl = T)
      source <- unlist(lapply(split_flow, `[[`, 1))
      sink <- unlist(lapply(split_flow, `[[`, 2))
      
      
      private$pSource <- source
      private$pSink <- sink
      private$pStocks <- stocks
      private$pBoundaries <- boundaries
      
      private$pFlowRate <- lapply(flowRate, function(x) {
        if(is.expression(x))
          x
        else
          parse(text = x)
      })
    },
    getOdeFunction = function() {
      ode <- private$makeFlowOdeFunction(private$pSource,
                                         private$pSink,
                                         private$pFlowRate)
      return(ode)
    },
    print = function() {
      cat("Flows:\n")
      for(flows in private$pFlows) {
        cat(flows)
        cat("\n")
      }
      mapply(function(source, sink, flowRate) {
        cat(paste0(source, " -> ", sink))
        cat(paste0("\t| Flow rate: ", as.character(flowRate)))
        cat("\n")
      }, private$pSource, private$pSink, private$pFlowRate)
      cat("\n")
      cat("Stocks:\n")
      for(stock in private$pStocks) {
        cat(stock)
        cat("\n")
      }
      cat("\n")
      cat("Boundaries:\n")
      for(boundary in private$pBoundaries) {
        cat(boundary)
        cat("\n")
      }
    },
    saveXml = function() { 
      doc = XML::newXMLDoc()
      rootOde <- XML::newXMLNode(class(self)[[1]], doc = doc)
      lOde <- list(
                   flows = VectorToCharDef(private$pFlows, TRUE),
                   flowRate = VectorToCharDef(private$pFlowRate),
                   stocks = VectorToCharDef(private$pStocks, TRUE),
                   boundaries = VectorToCharDef(private$pBoundaries))
      ListToXML(rootOde, lOde)
      invisible(rootOde)
    }
  ),
  
  active = list(),
  
  private = list(
    
    # Verifies the direction of the flow in list 'flows'. Returns a corrected list. 
    verifyInverseFlow = function(flows) { 
      if(length(grep("<-", flows))) { 
        aux_pos <- grep("<-", flows)
        aux_str <- strsplit(flows[aux_pos], "\\s+")
        for (i in 1:length(aux_pos)) 
          flows[aux_pos[i]] <- paste(aux_str[[i]][3], aux_str[[i]][1],
                                     sep = " -> ")
      }
      return(flows)
    },
    
    # Compares the variables in list 'flows' with the list 'boundaries' and 'st'. 
    # Gives a warning if there is no match.
    verifyBoundariesSt = function(boundaries, stocks, flows) { 
      strFlows <- strsplit(flows, "\\s+\\->\\s+")
      missingFlows <- list()
      for(i in 1:length(flows)) { 
        if(!(strFlows[[i]][1] %in% boundaries) && 
           !(strFlows[[i]][1] %in% stocks))
          missingFlows <- c(missingFlows, strFlows[[i]][1])
        if(!(strFlows[[i]][2] %in% boundaries) && 
           !(strFlows[[i]][2] %in% stocks))
          missingFlows <- c(missingFlows, strFlows[[i]][2])
      }
      
      missingFlows <- unique(missingFlows)
      
      for(e in missingFlows)
        warning(sprintf(auxiliaryMsg$sdMakeFlows6, e))
    },
    
    generateFlows = function(source, sink, stocks) {
      nStates <- length(stocks)
      
      sourceIdx <- sapply(source, function(x)
        match(x, stocks))
      sinkIdx   <- sapply(sink,   function(x)
        match(x, stocks))
      
      inflow <- list()
      outflow <- list()
      for (i in 1:nStates) {
        inflow[[i]] <- which(sinkIdx == i)
        outflow[[i]] <- which(sourceIdx == i)
      }
      
      return(list(inflow = inflow, outflow = outflow))
    },
    
    makeFlowOdeFunction = function(source, sink, flowRate) {
      flows <- private$generateFlows(source, sink, private$pStocks)
      
      ode <- function(t, st, ct, par, inp, sw, aux) {
        # Calc flow quantity
        flowQty <- sapply(flowRate, eval, envir = environment())
        
        # Calc differentials
        inflowQty <-
          sapply(flows$inflow, function(x)
            sum(flowQty[x]))
        outflowQty <-
          sapply(flows$outflow, function(x)
            sum(flowQty[x]))
        
        dS_dt <- inflowQty - outflowQty
        
        return(list(dS_dt))
      }
      
      return(ode)
    },
    
    pFlows = NULL,
    pSource = NULL,
    pSink = NULL,
    pFlowRate = NULL,
    pStocks = NULL,
    pBoundaries = NULL
  )
)