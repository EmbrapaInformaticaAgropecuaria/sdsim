sdOdeClass <- R6::R6Class(
  classname = "sdOde",
  
  public = list(
    getOdeFunction = function() {},
    print = function() {},
    saveXml = function() {}
  ),
  
  # active = list(),
  
  private = list(
    pOde = NULL
  )
)