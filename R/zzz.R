.onLoad <- function(libname, pkgname) 
{
  #options(digits = 10)  # increase digits precision
  msgs <- loadMessages("inst/messages/messages.xml")
  
  auxiliaryMsg <- msgs$auxiliaryMsg
  constructorsMsg <- msgs$constructorsMsg
  readInputDataMsg <- msgs$readInputDataMsg
  sdCoupledModelMsg <- msgs$sdCoupledModelMsg
  sdModelMsg <- msgs$sdModelMsg
  sdOdeModelMsg <- msgs$sdOdeModelMsg
  sdOutputMsg <- msgs$sdOutputMsg
  sdScenarioMsg <- msgs$sdScenarioMsg
  sdStaticModelMsg <- msgs$sdStaticModelMsg
  sdSimulatorMsg <- msgs$sdSimulatorMsg
}

.onUnload <- function(libname, pkgname)
{
  #options(digits = 7)  # set default
}