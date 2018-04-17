library(testthat)
context("Coupled Model instantiation and simulation")

test_that("Empty coupled model objet", code =
{
  m <- expect_is(sdCoupledModel("id"), "sdCoupledModel")
  expect_error(m$buildCoupledModel(),
               sprintf(sdCoupledModelMsg$buildCoupledModel1,
                                      m$id))
  expect_error(sdSimulate(m), sprintf(sdCoupledModelMsg$buildCoupledModel1,
                                      m$id))
  expect_warning(m$addComponent(2), sprintf(sdCoupledModelMsg$addComponent2,
                                            m$id, typeof(2)))
  expect_warning(m$removeComponent(2),
                 sprintf(sdCoupledModelMsg$removeComponent, m$id, "2"))

  expect_warning(m$addConnection(c(1, 2, 3, 4), c(1,2,3,4,"st$a")))

  expect_warning(m$addConnection(c(2, 2, 3, 4, "a"), c(1,2,3,4,"st$a")))

  expect_warning(m$removeConnection(4))
})

# test connection with state var
# test con with aux eq
# test con with algebraic eq
