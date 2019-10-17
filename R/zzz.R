.onLoad <- function(libname, pkgname) {
  #options(digits = 10)  # increase digits precision
  path = paste0(system.file(package = 'sdsim'), "/messages/messages.xml")
  msgs <- loadMessages(path)
  
  assign("auxiliaryMsg",      msgs$auxiliaryMsg, parent.env(environment()))
  assign("constructorsMsg",   msgs$constructorsMsg, parent.env(environment()))
  assign("readInputDataMsg",  msgs$readInputDataMsg, parent.env(environment()))
  assign("sdCoupledModelMsg", msgs$sdCoupledModelMsg, parent.env(environment()))
  assign("sdModelMsg",        msgs$sdModelMsg, parent.env(environment()))
  assign("sdOutputMsg",       msgs$sdOutputMsg, parent.env(environment()))
  assign("sdOdeModelMsg",     msgs$sdOdeModelMsg, parent.env(environment()))
  assign("sdScenarioMsg",     msgs$sdScenarioMsg, parent.env(environment()))
  assign("sdStaticModelMsg",  msgs$sdStaticModelMsg, parent.env(environment()))
  assign("sdSimulatorMsg",    msgs$sdSimulatorMsg, parent.env(environment()))
}

.onUnload <- function(libname, pkgname) {
  #options(digits = 7)  # set default
}