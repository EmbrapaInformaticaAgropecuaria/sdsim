Popover <- function(text, title, content, trigger = "hover click", 
                    placement = "bottom", tag = "a") {
  tags[[tag]](href = "#_",
              class = "pop",
              "title" = title,
              "data-content" = content,
              "data-trigger" = trigger,
              "data-placement" = placement,
              "data-toggle" = "popover",
              text)
}

# Custom function for creating ace editor with tabulation size parameter
AceEditorCustom <- function(varName, tabSize = 2, mode = "r", 
                            autoComplete = "enabled",
                            highlightActiveLine = T,
                            fontSize = 14,
                            useSoftTabs = T,
                            showLineNumbers = T, 
                            showPrintMargin = T,
                            wordWrap = F,
                            readOnly = F,
                            value = "",
                            theme = "crimson_editor",
                            height = "400px") {
  # Create ace editor
  editor <- shinyAce::aceEditor(varName, mode = mode, autoComplete = autoComplete, 
                      highlightActiveLine = highlightActiveLine, 
                      fontSize = fontSize, theme = theme,
                      showLineNumbers = showLineNumbers, height = height, 
                      wordWrap = wordWrap, readOnly = readOnly, value = value)
  varName <- paste0("editor__", varName)
  
  # Append setTabSize function
  editor[[3]][[3]] <- paste0(editor[[3]][[3]], varName, 
                             ".getSession().setTabSize(", tabSize,");")
  
  # Append setUseSoftTabs function
  if(useSoftTabs)
    editor[[3]][[3]] <- paste0(editor[[3]][[3]], varName, 
                               ".getSession().setUseSoftTabs(true);")
  
  # Append setShowPrintMargin function
  if(!showPrintMargin)
    editor[[3]][[3]] <- paste0(editor[[3]][[3]], varName, 
                               ".renderer.setShowPrintMargin(false);")
  
  return(editor)
}

# Example functions
DifferentialEquationsExampleStr <- "# Source: Soetaert_K.,_Cash_J.,_Mazzia_F.-Solving_Differential_ Equations_in_R-Springer(2012)
function (t, st, ct, par, inp, sw, aux) {
  # Calculate the differentials
  dy3 = st$y1 + 2 * st$dy2 - ct$mu2 * (st$y1 + ct$mu1)/aux$D1 - ct$mu1 * (st$y1 - ct$mu2)/aux$D2
  dy4 = st$y2 - 2 * st$dy1 - ct$mu2 * st$y2/aux$D1 - ct$mu1 * st$y2/aux$D2
  
  # Return the differentials to the integrator
  return(list(c(y1 = st$dy1, y2 = st$dy2, dy1 = dy3, dy2 = dy4)))
}"

parmInitExampleStr <- "# Source: Soetaert_K.,_Cash_J.,_Mazzia_F.-Solving_Differential_ Equations_in_R-Springer(2012)
function (st, ct, par, inp, sw, aux) {
  # Calculate new variable mu2 in constants list
  ct$mu2 = 1 - ct$mu1
  
  # Return altered state list and/or vars list
  return(list(st = st, ct = ct, inp = inp, par = par, sw = sw))
}"

staticParmInitExampleStr <- "# Source: Soetaert_K.,_Cash_J.,_Mazzia_F.-Solving_Differential_ Equations_in_R-Springer(2012)
function (st, ct, par, inp, sw, aux) {
  # Calculate new variable mu2 in constants list
  ct$mu2 = 1 - ct$mu1
  
  # Return altered state list and/or vars list
  return(list(st = st, ct = ct, inp = inp, par = par, sw = sw))
}"
rootExampleStr <- "# Source: Soetaert_K.,_Cash_J.,_Mazzia_F.-Solving_Differential_ Equations_in_R-Springer(2012)
function(t, st, ct, par, inp, sw, aux) {
  # 'Bounce' when height equals 0
  return(height)
}"

eventExampleStr <- "# Source: Soetaert_K.,_Cash_J.,_Mazzia_F.-Solving_Differential_ Equations_in_R-Springer(2012)
function (t, st, ct, par, inp, sw, aux) {
  # Set height to 0
  st$height = 0
  
  # Calculate speed after bounce
  st$speed = ct$elasticity * st$speed
  
  # Return the altered state
  return(st)
}"

globalExampleStr <- 'f1 = function(x) {
  return(x + 10)
}

f2 = function(x, y) {
  return(f1(x) * y)
}'

globalCallExampleStr <- '# Differential Equations function
function (t, st, ct, par, inp, sw, aux) {
  # Calling global function f2
  dy1 = f2(st$z, 2)
  
  return(list(c(dy1)))
}'