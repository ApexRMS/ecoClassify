## Unit tests for src/functions/0.0-setup.r

test_that("setCores returns 1 when multiprocessing is off", {
  source_ecoclassify_r("0.0-setup.r")

  sheet <- data.frame(EnableMultiprocessing = FALSE, MaximumJobs = 8L)
  expect_equal(setCores(sheet), 1)
})

test_that("setCores returns the requested jobs when they fit the machine", {
  source_ecoclassify_r("0.0-setup.r")

  sheet <- data.frame(EnableMultiprocessing = TRUE, MaximumJobs = 1L)
  expect_equal(setCores(sheet), 1L)
})

test_that("setCores caps requests above the available cores", {
  source_ecoclassify_r("0.0-setup.r")

  available <- parallel::detectCores()
  sheet <- data.frame(EnableMultiprocessing = TRUE, MaximumJobs = available + 10L)
  expect_equal(setCores(sheet), available - 1)
})
