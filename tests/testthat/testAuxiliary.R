context("Auxiliary Functions")

test_that("sdInitEquations Circular dependency", code =
{
  const <- list(c1 = 2, c2 = 1)
  vars <- list(k1 = 10, k2 = 2)

  aux <- expect_type(sdInitEquations(
    list("cDiffFrac <- (const$c1 - const$c2) / aux$kDiff",
         kDiff = "vars$k1 - vars$k2 + aux$firstaux",
         firstaux = "2"), eqName = 'aux'), "list")

  for (i in seq_along(aux))
    expect_type(aux[[i]] <- eval(aux[[i]]), "double")

})

test_that("time Series point to temporal function methods", code =
{
  datapoints <- data.frame(time = c(0,5,10), value = c(10,50,100))
  dataperiodic <- data.frame(time = c(0,5,10), value = c(10,50,10))
  expect_is(sdTemporalFunction(x = datapoints, method = "constant"), "function")
  expect_is(sdTemporalFunction(x = datapoints, method = "linear"), "function")
  expect_is(sdTemporalFunction(x = datapoints, method = "fmm"), "function")
  expect_is(sdTemporalFunction(x = dataperiodic, method = "periodic"),
            "function")
  expect_is(sdTemporalFunction(x = datapoints, method = "monoH.FC"), "function")
  expect_is(sdTemporalFunction(x = datapoints, method = "hyman"), "function")
})