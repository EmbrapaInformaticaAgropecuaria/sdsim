# Reserved words that can cause false positive matches when a regex is applyed 
# in a coupling
sdsimReserved <- c("t", "st", "ct", "par", "inp", "sw", "aux", "eq", 
                   "interpolation_", "fun_")

#' sdsim Labeling Rules
#' 
#' A string is used to identify the models and the scenario variables names in 
#' the sdsim package. 
#' 
#' The model id is used to uniquely distinguish components when coupling and is 
#' used inside regex patterns and as lists names when building a coupled model.
#' The scenario variables are used inside regex patterns to build a coupled 
#' model scenario.
#' 
#' To prevent errors the package automatically format the models id and the 
#' scenario variables names into valid names following the criterias:
#' 
#' \itemize{
#' \item whitespace: all whitespace characters are removed using 
#' \code{\link[base]{grep}};
#' \item valid names: use the function \code{\link[base]{make.names}} to make 
#' syntactically valid names in R;
#' \item sdsim reserved word: do not accept the words "t", "st", "ct", "par", 
#' "inp", "sw", "aux", "eq", "interpolation_" or "fun_".
#' }
#' 
#' If the model id or the variables names is: missing, not a string or a 
#' sdsim reserved word, it will be discarted. For the models id, in any of these 
#' cases, a timestamp will be created using the name of the model class
#' concatenated with the \code{\link[base]{Sys.time}} value (current day) to be 
#' used instead. 
#' 
#' @name sdsim-LabelingRules
#' @aliases sdsim-LabelingRules
sdsimModelLabelingRules <- NULL

#' Initialize R-format Equations from a List of Strings or Expressions
#' 
#' Initialize a list of equations from a list of strings or R-expressions with 
#' the equations in R-format and sort the equations using their 
#' interdependencies to enable the reuse of evaluated equations in other 
#' equations further in the list. 
#' Abort if any circular dependency is found and warn the user.
#' Each equation will be an element of the returned sorted list in R-expression 
#' format. Use the \code{\link[base]{eval}} function to evaluate each 
#' R-expression in a specified environment.
#' 
#' The returned list of equations can be: added to the auxiliary equations list
#' of a \code{\link{sdOdeModelClass}} object to replace the use of functions to 
#' calculate intermediary values of the model's system of differential 
#' equations; 
#' or added to the algebraic equations of a \code{\link{sdStaticModelClass}} 
#' object. 
#' 
#' @param equations A list of strings or R-expressions with the equations in 
#' R-format.
#' @param separator The assignment symbol used in the \code{equations} strings. 
#' Default is "<-".
#' @param eqName The name of the list that will be initialized and sorted, e.g.
#' 'aux' for \code{\link{sdOdeModelClass}} auxiliary equations and/or 'eq' for 
#' \code{\link{sdStaticModelClass}} algebraic equations. Default is 'aux'.
#' 
#' Can be a vector with all the equations list names that should be considered 
#' in the sorting. Default is c('aux', 'eq').
#' @return A sorted list of equations in R-expression format.
#' @examples 
#' const <- list(c1 = 2, c2 = 1)
#' vars <- list(k1 = 10, k2 = 2)
#' 
#' auxEquationsStrings <- list("cDiffFrac <- (const$c1 - const$c2) / aux$kDiff",
#'                             kDiff = "vars$k1 - vars$k2")
#' aux <- sdInitEquations(auxEquationsStrings, eqName = 'aux')
#' 
#' print(aux)
#' #> $kDiff
#' #> expression(vars$k1 - vars$k2)
#' #>
#' #> $cDiffFrac
#' #> expression((const$c1 - const$c2) / aux$kDiff)
#' 
#' aux$kDiff <- eval(aux$kDiff)
#' print(eval(aux$cDiff))
#' #> [1] 0.125
sdInitEquations <- function (equations, separator = "<-", 
                             eqName = "aux") { 
  equationList <- list()
  
  # If equations is a list containing expressions and/or characters
  if (is.list(equations) && length(equations) > 0) { 
    # Process separator parameter regex to accept spaces
    separator <- paste0("[ \t\n]*", separator, "[ \t\n]*")
    nameList <- names(equations)
    
    for (i in 1:length(equations)) { 
      
      eq <- equations[[i]]
      if (is.character(eq)) { 
        # If the equation has variable attribution
        if (grepl("<-", eq)) { 
          # Splits the equation into resulting variable and body
          eqSplit <- strsplit(sub(separator, "\01", eq), "\01")
          equationBody <- eqSplit[[1]][[2]]
          equationVariable <- gsub("[ \t]*", "", eqSplit[[1]][[1]])
          
          # check if element is named
          if (equationVariable == "") { 
            warning(sprintf(auxiliaryMsg$sdInitEq1, eqName, equationBody), 
                    call. = FALSE)
            next()
          }
          
          # Assemble the expression
          equation <- parse(text = equationBody)
        } else { 
          # If the variable correspondant to the equation is the element name
          # in the list
          
          # check if element is named
          if (is.null(nameList) || nameList[[i]] == "") { 
            warning(sprintf(auxiliaryMsg$sdInitEq1, eqName, eq), call. = FALSE)
            next()
          }
          
          equationBody <- eq
          equationVariable <- nameList[[i]]
          
          # Assemble the expression
          equation <- parse(text = equationBody)
        }
        
      } else if (is.expression(eq) || is.language(eq)) { 
        # check if element is named
        if (is.null(nameList) || nameList[[i]] == "") { 
          warning(sprintf(auxiliaryMsg$sdInitEq1, eqName, eq), call. = FALSE)
          next()
        }
        
        equationVariable <- nameList[[i]]
        equation <- eq
      } else { 
        # TODO: Vitor
        # auxiliaryMsg$sdInitEq(paste(eqName, collapse = " and "), eq)
        warning(sprintf(auxiliaryMsg$sdInitEq, 
                        paste(eqName, collapse = " and "), typeof(eq)))
        next()
      }
      
      equationList[[equationVariable]] <- equation
    }
  }
  
  return(topologicalSortEquations(as.list(equationList), eqName = eqName))
}

# Sort the auxiliary equations by dependency
topologicalSortEquations <- function(equations, eqName = c("aux","eq")) { 
  dependents <- list() # store the dependents of each eq
  eqdegree <- list() # store the dependency degree of each eq
  eqorder <- c() # store the eqs in the right order
  eqfree <- c() # store the eqs free of dependency
  
  # build regex to find the eqName usage
  # greb all varNames of the form: eqName$varName | eqName['varName | 
  # eqName[['varName with ' or "
  patterneq <- paste0("((?<=", eqName[[1]], "\\$|", eqName[[1]], "\\[\\'|", 
                      eqName[[1]], "\\[\\\"|", eqName[[1]], "\\[\\", 
                      "[\\'|", eqName[[1]], "\\[\\[\\\")\\w+(\\.\\w*)*)")
  if (length(eqName) > 1) { 
    for (i in 2:length(eqName))
      patterneq <- paste0(patterneq, "|","((?<=", eqName[[i]], "\\$|", 
                          eqName[[i]], "\\[\\'|", eqName[[i]], 
                          "\\[\\\"|", eqName[[i]],"\\[\\", 
                          "[\\'|", eqName[[i]], "\\[\\[\\\")\\w+(\\.\\w*)*)")
  }
  
  # build the equations dependent lists
  for (eqVar in names(equations)) { 
    # get the dependency list of each eq and set its degree
    dependency <- unique(unlist(regmatches(
      toString(equations[[eqVar]]), 
      gregexpr(pattern = patterneq, toString(equations[[eqVar]]), perl = T))))
    
    eqdegree[[eqVar]] <- length(dependency) 
    
    # add to the free list the eqs with no dependency
    if (eqdegree[[eqVar]] == 0)
      eqfree <- c(eqfree, eqVar)
    
    # build the dependents eq    
    for (d in dependency)
      dependents[[d]] <- c(dependents[[d]], eqVar)
  }
  
  # perform the topological ordering while there are free equaitons left
  while (!is.null(eqfree[1]) && !is.na(eqfree[1])) { 
    # put the firs free eq in the order list and remove it from the free list
    eqVar <- eqfree[[1]]
    eqorder <- c(eqorder, eqVar)
    eqfree <- eqfree[-1]
    
    # decrease the dependents equations degree
    for (e in dependents[[eqVar]]) { 
      eqdegree[[e]] <- eqdegree[[e]] - 1
      # add the new free eqs
      if (eqdegree[[e]] == 0)
        eqfree <- c(eqfree, e)
    }
  }
  
  # check for circular dependency
  if (length(eqorder) != length(equations)) { 
    auxiliaryMsg$topologicalSortEq(eqName, equations, eqorder, dependents)
    return(list())
  }
  
  return(equations[eqorder])
}

# Transform a List of Time Series Variables into a List of Temporal Functions 
# 
# Uses the \code{\link{sdTemporalFunction}} function to interpolate all the 
# time series elements of the \code{x} list across the given time sequence. 
# It accepts a list of fixed values, matrices, data.frames or file names, and 
# returns a list of functions performing the linear, constant and/or spline 
# interpolation of it's elements. If an interpolation fails the given element
# is not included in the returned list.
# See details for more informations about the accepted types.
# 
# The time series variables are the model parameters that vary over time, e.g.
# a diet that change after a period of time. This function helps to transform 
# this kind of input variables into unary time functions.
# 
# What happens when a element of \code{x} is a:
# \itemize{
#   \item fixed value: The fixed value is repeated for every time step.
#   \item matrix or data.frame: The column \code{colTimes} (default is 1) 
#   should contain the time sequence and the column \code{colValues} (default 
#   is 2) should contain the values for each given time.
#   \item character: It is interpreted as a text file name. This file must 
#   be inside the current working directory or inside the path of
#   \code{timeSeriesDirectory}. The file should have 2 columns, the column 
#   \code{colTimes} (default is 1) should contain the time sequence and the 
#   column \code{colValues} (default is 2) should contain the values for each 
#   given time
# }
# 
# @param x A list with time series variables. These variables can be a fixed 
# value, a matrix, a data.frame or a character string with a 
# text file name. See details for more information.
# @param colTimes (Optional) The number or name of the column that contains the 
# time sequence, when an element of \code{x} is a matrix, a data.frame or 
# a file name. 
# The default value is 1. 
# @param colValue (Optional) The number or name of the column that contains the 
# time series values, when an element of \code{x} is a matrix, a data.frame or 
# a file name. 
# The default value is 2.
# @param methods A list with the interpolation methods to be used. Choices are: 
# "linear" or "constant" for the \code{\link[stats]{approxfun}} function 
# performing the interpolation; or, "fmm", "natural", "periodic", "monoH.FC" 
# or "hyman" for the \code{\link[stats]{splinefun}} function performing the 
# interpolation.
# @param sep The field separator character used in the input files, when an 
# element of \code{x} is a text file name. Default is ",".
# @param dec The decimal point character used in the input files, when an 
# element of \code{x} is a text file name. Default is ".".
# @param header a logical value indicating whether the file contains the names 
# of the variables as its first line. If missing, the value is determined from 
# the file format: header is set to TRUE if and only if the first row contains 
# one fewer field than the number of columns. To be passed to the 
# \code{\link[utils]{read.table}} function.
# @param timeSeriesDirectory The path to the directory where the time series 
# text data files are stored, when an element of \code{x} is a text file name.
# @return A list of functions performing the linear, constant or spline 
# interpolation of the time series variables. The list is named using the 
# \code{x} names.
sdTemporalFunctionList <- function(x, colTimes = 1, colValue = 2, 
                                   methods, 
                                   sep = ",", dec = ".", header = TRUE,
                                   timeSeriesDirectory = "") { 
  if (length(x) == 0)
    return(x)
  
  if (length(x) != length(methods)) { 
    warning(sprintf(auxiliaryMsg$sdTemporalFunctionList))
    return(NULL)
  }
  
  # transform the time series into temporal functions
  timeSeriesFun <- lapply(1:length(x), FUN = function(i) {  
    sdTemporalFunction(x = x[[i]], colTimes = colTimes, 
                       colValue = colValue, 
                       method = methods[[i]], sep = sep, dec = dec, 
                       timeSeriesDirectory = timeSeriesDirectory,
                       header = header) 
  })
  names(timeSeriesFun) <- names(x)
  
  # remove NULL values
  timeSeriesFun <- timeSeriesFun[names(timeSeriesFun) %in%
                                   names(unlist(timeSeriesFun))]
  
  return(timeSeriesFun)
}

#' Transform Time Series Variables into Temporal Functions 
#' 
#' The time series variables are the model variables that vary over time, e.g.
#' a diet that change after a period of time, meteorological data, etc. This 
#' function helps to transform this kind of input variables into temporal 
#' functions and it is automatically used by the \code{\link{sdScenarioClass}}. 
#' It accepts as inputs fixed values, matrices, data.frames or 
#' text file names, and returns a function performing the linear, constant or 
#' spline interpolation with extrapolation outside of the time interval. 
#' See details for more informations about the accepted 
#' types.
#' 
#' Uses the \code{\link[stats]{approxfun}} or the \code{\link[stats]{splinefun}}
#' function to interpolate the time series values across the given time 
#' sequence.  
#' 
#' What happens when \code{x} is a:
#' \itemize{
#'   \item fixed value: The fixed value is repeated for any time step.
#'   \item matrix or data.frame: The column \code{colTimes} (default is 1) 
#'   should contain the time sequence and the column \code{colValues} (default 
#'   is 2) should contain the values for each given time. Both columns are 
#'   numeric.
#'   \item character: It is interpreted as a text file name. This file must 
#'   be inside the current working directory or inside the 
#'   \code{timeSeriesDirectory}. The file should have 2 columns, the column 
#'   \code{colTimes} (default is 1) should contain the time sequence and the 
#'   column \code{colValues} (default is 2) should contain the values for each 
#'   given time. Both columns are numeric.
#' }
#'  
#' @param x The time series variable value. It can be a fixed value, a matrix, 
#' a data.frame or a character string with a file name. See details for more
#' information.
#' @param colTimes (Optional) The number or name of the column that contains the 
#' time sequence, when an element of \code{x} is a matrix, a data.frame or 
#' a text file name. 
#' The default value is 1. 
#' @param colValue (Optional) The number or name of the column that contains the 
#' time series values, when an element of \code{x} is a matrix, a data.frame or 
#' a text file name. 
#' The default value is 2.
#' @param method Specifies the interpolation method to be used. Choices are: 
#' "linear" or "constant" for an \code{\link[stats]{approxfun}} function 
#' performing the interpolation; 
#' or, "fmm", "natural", "periodic", "monoH.FC" or "hyman" for a 
#' \code{\link[stats]{splinefun}} function performing the interpolation.
#' @param sep The field separator character used in the data files, when 
#' \code{x} is a file name. Default is ",".
#' @param dec The decimal point character used in the data files, when \code{x} 
#' is a file name. Default is ".".
#' @param header a logical value indicating whether the file contains the names 
#' of the variables as its first line. If missing, the value is determined from 
#' the file format: header is set to TRUE if and only if the first row contains 
#' one fewer field than the number of columns. To be passed to the 
#' \code{\link[utils]{read.table}} function. Default is \code{TRUE}.
#' @param timeSeriesDirectory The path to the directory where the time series 
#' data files are stored, when \code{x} is a character file name.
#' @return A function performing the linear, constant or spline interpolation of 
#' the time series points or \code{NULL} if an error occurs.
#' @examples
#' # Diet in a data.frame with constant interpolation 
#' diet <- data.frame(time = c(0,5,10), value = c(10,50,100))
#' dietFun <- sdTemporalFunction(x = diet, method = "constant") 
#' 
#' dietFun(2)
#' #> [1] 10
#' dietFun(7)
#' #> [1] 50
#' dietFun(10)
#' #> [1] 100
sdTemporalFunction <- function(x, colTimes = 1, colValue = 2, 
                               method = "linear", 
                               sep = ",", dec = ".", header = TRUE,
                               timeSeriesDirectory = "") {  
  splineMethods <- c("fmm", "natural", "periodic", "monoH.FC" , "hyman")
  approxfunMethods <- c("linear", "constant")
  
  if (length(x) == 0) # empty object
    return(x)
  
  if (is.numeric(x) && (length(x) == 1)) { ## its a fixed value
    timeS <- c(0, 1)
    values <- c(x, x)
  } else if (is.matrix(x) || is.data.frame(x)) { ## its a matrix or data frame   
    # check if the indexes are valid
    if (!all(c(colTimes, colValue) <= ncol(x))) { 
      warning(auxiliaryMsg$sdTemporalFunction6)
      return(NULL)
    }
    
    timeS <- x[, colTimes]
    values <- x[, colValue]  # column col must have the values for each time
  } else if (is.list(x)) { 
    # check if the indexes are valid
    if (!all(c(colTimes, colValue) <= length(x))) { 
      warning(auxiliaryMsg$sdTemporalFunction6)
      return(NULL)
    }
    
    timeS <- x[colTimes]
    values <- x[colValue]  # column col must have the values for each time
  } else if (is.character(x)) {  ## its a character file name - read from file
    if (timeSeriesDirectory != "")
      timeSeriesDirectory <- paste0(timeSeriesDirectory, "/")
    if (!file.exists(paste0(timeSeriesDirectory, x))) { 
      warning(sprintf(auxiliaryMsg$sdTemporalFunction1,paste0(timeSeriesDirectory, x)))
      return(NULL)
    }
    
    temporal_data <- tryCatch( { 
      read.table(file = paste0(timeSeriesDirectory, x), 
                 colClasses = c("numeric", "numeric"), 
                 dec = dec, sep = sep, 
                 strip.white = TRUE, stringsAsFactors = FALSE,
                 header = header)},
      error = function(e) { 
        warning(sprintf(auxiliaryMsg$sdTemporalFunction2, paste0(timeSeriesDirectory, x), e))
        return(NULL)
      })
    
    if (is.null(temporal_data))
      return(NULL)
    
    if (!all(c(colTimes, colValue) <= ncol(x))) { 
      warning(auxiliaryMsg$sdTemporalFunction6)
      return(NULL)
    }
    
    # first column must have the time seq
    timeS  <- temporal_data[, colTimes]    
    # second column must have the values for each time
    values <- temporal_data[, colValue]  
  } else {
    warning(sprintf(auxiliaryMsg$sdTemporalFunction3,typeof(x)))
    return(NULL)
  }
  
  if (length(timeS) == 1) { # is a constant value
    timeS <- c(timeS, timeS)
    values <- c(values, values)
    method <- "constant"
  }
  
  # values to extrapolate the times interval
  yl <-  values[1]
  yr <- values[length(values)]
  
  # check the interpolation type given the wanted method
  if (method %in% approxfunMethods) { 
    f <- tryCatch( { 
      stats::approxfun(x = timeS, y = values, yleft = yl, yright = yr, 
                       method = method)
    },
    error = function(e) { 
      warning(sprintf(auxiliaryMsg$sdTemporalFunction5,e))
      return(NULL)
    })
  } else if (method %in% splineMethods) { 
    f <- tryCatch( { 
      stats::splinefun(x = timeS, y = values, method = method)
    },
    error = function(e) { 
      warning(sprintf(auxiliaryMsg$sdTemporalFunction5,e))
      return(NULL)
    })
  } else { 
    warning(sprintf(auxiliaryMsg$sdTemporalFunction4))
    f <- NULL
  }
  
  return(f)
}

# source: https://stackoverflow.com/questions/6256064/
# how-to-create-xml-from-r-objects-e-g-is-there-a-listtoxml-function
# Adapted to transform a list of lists in to a XML node tree
ListToXML <- function(node, sublist) { 
  # vectors leafs
  if (is.numeric(sublist) || is.character(sublist)) { 
    child <- XML::newXMLNode(names(sublist)[i], parent=node)
    
    if (is.numeric(sublist[[i]]))
      XML::xmlValue(child) <- VectorToCharDef(sublist[[i]]) 
    else # quote
      XML::xmlValue(child) <- VectorToCharDef(sublist[[i]], quote = T) 
    return(node)
  }
  
  # list
  for (i in 1:length(sublist)) { 
    child <- XML::newXMLNode(names(sublist)[i], parent=node)
    
    if (typeof(sublist[[i]]) == "list" && length(sublist[[i]]) > 0) { 
      ListToXML(child, sublist[[i]])
    } else if (length(sublist[[i]]) > 1) { # to store vectors
      if (is.numeric(sublist[[i]]))
        XML::xmlValue(child) <- VectorToCharDef(sublist[[i]]) 
      else # quote
        XML::xmlValue(child) <- VectorToCharDef(sublist[[i]], quote = T) 
    } else { 
      XML::xmlValue(child) <- sublist[[i]]
    }
  }
  
  return(node)
}

VectorToCharDef <- function(x, quote = F) { 
  if (quote)
    return(paste0("c(", paste0("'", x, "'", collapse = ","), ")"))
  else
    return(paste0("c(", paste0(x, collapse = ","), ")"))
}

FunToString <- function(fun) { 
  if (!is.null(fun))
    return(paste(format(fun), collapse = "\n"))
  else
    return(NULL)
}

StringToFun <- function(str) { 
  if (!is.null(str))
    return(eval(parse(text = str)))
  else
    return(NULL)
}

# source: https://stackoverflow.com/questions/9519543/merge-two-lists-in-r
appendList <- function (x, val) { 
  stopifnot(is.list(x), is.list(val))
  xnames <- names(x)
  for (v in names(val)) {
    x[[v]] <- if (v %in% xnames && is.list(x[[v]]) && is.list(val[[v]])) 
      appendList(x[[v]], val[[v]])
    else c(x[[v]], val[[v]])
  }
  x
}

# Source: package R6 print.R
# Given a string, indent every line by some number of spaces.
# The exception is to not add spaces after a trailing \n.
indent <- function(str, indent = 0) {
  gsub("(^|\\n)(?!$)",
       paste0("\\1", paste(rep(" ", indent), collapse = "")),
       str,
       perl = TRUE)
}

# replace variable names inside the function or the list of expressions with 
# the standard modelId.variableName, where modelId. is the prefix added
replaceCoupledVarsNames <- function(func, listExp, componentsVarNames, modelId) { 
  if (!missing(func) && is.function(func)) { 
    funstr <- FunToString(func)
    for (var in componentsVarNames) { 
      funstr <- gsub(pattern = paste0("(?<!\\.)\\b", var, "\\b(?!\\.)"), 
                     replacement = paste0(modelId, ".", var), x = funstr, 
                     perl = T)
    }
    
    func <- StringToFun(funstr)
    return(func)
  } else if (!missing(listExp) && is.list(listExp)) { 
    expstr <- lapply(listExp, function(x) toString(as.expression(x)))
    
    for (var in componentsVarNames) { 
      for (varexp in names(listExp))
        expstr[[varexp]] <- gsub(pattern = paste0("(?<!\\.)\\b", var, 
                                                  "\\b(?!\\.)"), 
                                 replacement = paste0(modelId, ".", var), 
                                 x = expstr[[varexp]], 
                                 perl = T)
    }
    
    listExp <- lapply(expstr, function(x) parse(text = x))
    return(listExp)
  }
  
  return(NULL)
}

# merge the default list with a possibly incomplete list
MergeLists <- function(parm, defaultParm, listName = "var") { 
  if (is.null(parm))
    return(defaultParm)
  
  # warningList <- names(parm)[!(names(parm) %in% c(names(defaultParm), 
  #                                                 "interpolation_", "fun_"))]
  # lapply(warningList, function(x)
  # {
  #   auxiliaryMsg$MergeLists(x, listName)
  # })
  
  # update default parms values with given parm
  for (tag in names(parm))
    defaultParm[[tag]] <- parm[[tag]]
  
  return(defaultParm)
}

# Merge environment e1 and e2 into one: e1 <- unique(e1 + e2)
# e2 vars will overwritte e1 vars with the same name
appendEnv = function(e1, e2, prefix = NULL) { 
  listE1 <- ls(e1)
  listE2 <- ls(e2)
  for(v in listE2) { 
    #if (v %in% listE1) warning(sprintf("Variable %s is in e1, too!", v))
    if (is.null(prefix))
      e1[[v]] <- e2[[v]]
    else
      e1[[paste0(prefix, ".", v)]] <- e2[[v]]
  }
}

#' The sdsim Shiny Server Interface
#' 
#' Runs the sdsim Shiny server graphical user interface. Open the given http in 
#' your browser to access the server interface. 
#' Uses the \code{\link[shiny]{runApp}} function to launch the sdsim 
#' application.
#' 
#' @param launch.browser If true, the system's default web browser will be 
#' launched automatically after the app is started. Defaults to true in 
#' interactive sessions only. The value of this parameter can also be a 
#' function to call with the application's URL.
#' @param port The TCP port that the application should listen on. If the port 
#' is not specified, and the shiny.port option is set (with options(shiny.port = 
#' XX)), then that port will be used. Otherwise, use a random port.
#' @param host The IPv4 address that the application should listen on. Defaults 
#' to the shiny.host option, if set, or "127.0.0.1" if not. See 
#' \code{\link[shiny]{runApp}} Details.
#' @examples
#' ## Open the sdsim user interface using the given http
#' #> sdRunApp()
sdRunApp <- function(launch.browser = T, port = getOption("shiny.port"),
                     host = getOption("shiny.host", "127.0.0.1")) { 
  shiny::runApp(system.file(appDir = "application", package = "sdsim"), 
                launch.browser = launch.browser, port = port, host = host)
}

#' sdsim Models Repository List
#' 
#' Show the complete list of models stored in the package sdsim repository.
#' Use the function \code{\link{sdLoadModel}} with \code{file = 'modelID'} and 
#' \code{repository = TRUE} to load a model from the sdsim repository.
#' 
#' Models ID and description:
#' \describe{
#' \item{\code{Arenstorf}}{The Arenstorf problem, from Astronomy, describes the 
#' movement of a small body orbiting regularly around two larger objects, such 
#' as a spacecraft going between the Earth and the Moon. 
#' 
#' The two large bodies have mass m1 and m2 and move in a circular rotation 
#' (coordinates y1 and y2) in a plane, while the third body has negligible mass 
#' and is moving in the same plane.
#' 
#' It was necessary to solve this problem in order to determine the path that 
#' the Apollo spacecraft had to take in its journey between the Earth and the 
#' Moon. The problem was solved by Arenstorf and now it is an often used test 
#' problem for non-stiff solvers.
#' 
#' Source: Soetaert K., Cash J., Mazzia F. - Solving Differential Equations in 
#' R - Springer(2012)}
#' \item{\code{BouncingBall}}{Simulates a bouncing ball specified by its 
#' position above the ground (height). The ball is thrown vertically, from the 
#' ground (height(0) = 0 meter), with initial velocity (speed) of 10 meter / 
#' second. 
#' 
#' As the ball hits the ground, it bounces. This causes a sudden change in the 
#' value of the ball's velocity (a sign-reversal and reduction of its magnitude 
#' directly proportional to the coefficient of restitution (k)).
#' 
#' Source: Soetaert K., Cash J., Mazzia F. - Solving Differential Equations in 
#' R - Springer(2012)}
#' \item{\code{Customer}}{In order to demonstrate how a system dynamics model is 
#' constructed, a one-stock model of an organization's customer base is modeled. 
#' 
#' Given that the customer base is an accumulation, it can be modeled as a 
#' stock. The inflow is recruits, and the outflow are losses, also known as the 
#' churn rate. The goal of organizations is to limit the losses and maximize the 
#' recruits, in order to maintain increasing customers levels, and therefore 
#' support company growth. 
#' 
#' Source: Jim Duggan, System Dynamics Modeling with R (2016).}
#' \item{\code{GrowthConstrained}}{The model captures the growth and decline 
#' dynamics of a company discovering a new oil field, where the stock of oil 
#' could potentially last for up to 200 years. 
#' 
#' Source: Jim Duggan, System Dynamics Modeling with R (2016).}
#' \item{\code{RigidBody}}{A standard test problem for non-stiff solvers, as 
#' proposed by Krogh. It describes the Euler equations of a rigid body without 
#' external forces.
#' The three dependent 
#' variables (x , y , z) are the coordinates of the rotation 
#' vector, while I1, I2 and I3 are the principal moments of inertia.
#' 
#' Source: Soetaert K., Cash J., Mazzia F. - Solving Differential Equations in 
#' R-Springer(2012)}
#' }
#' @format NULL
#' @examples
#' ## Show the models ID present in the sdsim repository
#' sdRepository()
#' 
#' ## Load the Arenstorf model from the sdsim repository 
#' arenstorf <- sdLoadModel(file = "Arenstorf", repository = TRUE) 
#' 
#' # validate the model, simulate it and plot the coordinates result
#' arenstorf$verifyModel(verbose = TRUE)
#' outaren <- sdSimulate(arenstorf)
#' outaren$plot("y1 ~ y2")
sdRepository <- function() { 
  modelsRep <- gsub(pattern = "\\.xml", replacement = "", 
                    x = list.files(system.file(appDir = "repository/", 
                                               package = "sdsim"), 
                                   pattern = "*.xml"))
  
  return(modelsRep)
}

# return the sdsim package XML prefix
xmlPrefix <- function() { 
  return(paste0("<?sdsim about='R package for ",
                "modeling and simulation of system dynamics'",
                " version='",
                toString(utils::packageVersion("sdsim")),
                "' date='", Sys.Date(),
                "'?>"))
}

#' Create a Scenario EXCEL Template
#' 
#' Create a sdsim scenario excel template to be filled by the user and loaded
#' with the \code{\link{sdLoadScenario}} function. The empty sheets that will
#' not be used should be deleted before loading the scenario to prevent 
#' warnings.
#' 
#' @param file A string with the name of the excel file. The file extension
#' must be included in the file name, e.g. '.xlsx'.
#' @param colWidth The Excel columns width.
#' @examples 
#' # create a scenario EXCEL template
#' sdExcelTemplate("test.xlsx")
#' 
#' # edit the created EXCEL file and load it
#' scenTest <- sdLoadScenario("test.xlsx")
#' print(scenTest)
sdExcelTemplate <- function(file = "Scenario.xlsx", 
                            colWidth = c(10, 10, 10, 30, 10)) { 
  inputData <- list(state = data.frame(Variable = c(""), Value = c(""), 
                                       Unit = c(""), Description = c("")), 
                    constant = data.frame(Variable = c(""), Value = c(""), 
                                          Unit = c(""), Description = c("")), 
                    parameter = data.frame(Variable = c(""), Value = c(""), 
                                           Unit = c(""), Description = c("")), 
                    input = data.frame(Variable = c(""), Value = c(""), 
                                       Unit = c(""), Description = c(""), 
                                       Interpolation = c("")), 
                    switch = data.frame(Variable = c(""), Value = c(""), 
                                        Unit = c(""), Description = c("")), 
                    simulation = data.frame(Variable = c("id", "from", 
                                                         "to", "by", "method"), 
                                            Value = c(paste("scenario", 
                                                            Sys.Date()),
                                                      "","","",""),
                                            Unit = c("","","","",""), 
                                            Description = c("","","","","")))
  
  # Save to excel
  wb <- openxlsx::createWorkbook()
  lapply(names(inputData), function(x) { 
    openxlsx::addWorksheet(wb, sheetName = x)
    openxlsx::writeDataTable(wb = wb, sheet = x, x = inputData[[x]], 
                             colNames = T)
    openxlsx::setColWidths(wb = wb, sheet = x, cols = 1:5, 
                           widths = colWidth)
  })
  
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  
  invisible(inputData)
}

#' Create a list of equations
#' 
#' 
#' This function converts equations written in plain R code into a list of 
#' equations in chracter format that can be recognized by the sdsim functions 
#' \code{\link{sdOdeModel}} and \code{\link{sdStaticModel}}.
#' 
#' @param ... One or more equations. The equations must have the format
#' <equation_name> = <equation>, where equation_name is the name of the 
#' equation, and equation is the body of the equation. The equations must
#' written in plain R code.
#' @return A list of strings containing the equations in chracter format.
#' @examples 
#' # Create equations list
#' equations <- sdEquationsList(
#'   x = a + b,
#'   y = c ^ 2
#' )
sdEquationList <- function(...) { 
  # Convert parameters to string
  eqList <- as.list(sapply( substitute(list(...)), deparse)[-1])
  eqList <- lapply(eqList, function(x) { 
    # Collapse sublists
    x <- paste(x, collapse = " ")
    # Remove extra spaces
    x <- gsub(pattern = "[ ]{2,}", replacement = " ", x)
  })
  return(eqList)
}

# Merge the a default scenario with an alternate scenario. Returns the
# merged scenario.
mergeScenarios <- function(defaultScenario, alternateScenario, verbose) { 
  if (length(alternateScenario$state) > 0)
    defaultScenario$addState(alternateScenario$state, verbose = verbose)
  if (length(alternateScenario$constant) > 0)
    defaultScenario$addConstant(alternateScenario$constant, verbose = verbose)
  if (length(alternateScenario$input) > 0)
    defaultScenario$addInput(
      alternateScenario$input[!(names(alternateScenario$input) %in% c("interpolation_", 
                                                                      "fun_"))],
      interpolation = alternateScenario$input[["interpolation_"]],
      verbose = verbose)
  if (length(alternateScenario$parameter) > 0)
    defaultScenario$addParameter(alternateScenario$parameter, verbose = verbose)
  if (length(alternateScenario$switch) > 0)
    defaultScenario$addSwitch(alternateScenario$switch, verbose = verbose)
  if (!is.null(alternateScenario$times))
    defaultScenario$times <- alternateScenario$times
  if (!is.null(alternateScenario$method))
    defaultScenario$method <- alternateScenario$method
  
  return(defaultScenario)
}