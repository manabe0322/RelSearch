test_that("calc_mu_step pattern 1", {

  # Condition
  val <- 15
  ral <- 16

  # Run
  mu_step <- calc_mu_step(val, ral)

  # Test
  expect_equal(mu_step, 1)
})

test_that("calc_mu_step pattern 2", {

  # Condition
  val <- 15
  ral <- c(17, 18)

  # Run
  mu_step <- calc_mu_step(val, ral)

  # Test
  expect_equal(mu_step, 2)
})

test_that("calc_mu_step pattern 3", {

  # Condition
  val <- 15
  ral <- c(15, 17)

  # Run
  mu_step <- calc_mu_step(val, ral)

  # Test
  expect_equal(mu_step, 0)
})

test_that("calc_mu_step pattern 4", {

  # Condition
  val <- 15
  ral <- 15

  # Run
  mu_step <- calc_mu_step(val, ral)

  # Test
  expect_equal(mu_step, 0)
})

test_that("calc_mu_step pattern 5", {

  # Condition
  val <- 15
  ral <- 15.1

  # Run
  mu_step <- calc_mu_step(val, ral)

  # Test
  expect_equal(mu_step, 99)
})

test_that("calc_mu_step pattern 6", {

  # Condition
  val <- 15
  ral <- c(15.1, 16)

  # Run
  mu_step <- calc_mu_step(val, ral)

  # Test
  expect_equal(mu_step, 1)
})

test_that("calc_mu_step pattern 7", {

  # Condition
  val <- c(15, 17)
  ral <- c(16, 18)

  # Run
  mu_step <- calc_mu_step(val, ral)

  # Test
  expect_equal(mu_step, 99)
})

test_that("calc_mu_step pattern 8", {

  # Condition
  val <- c(15, 17)
  ral <- c(15, 18)

  # Run
  mu_step <- calc_mu_step(val, ral)

  # Test
  expect_equal(mu_step, 1)
})

test_that("calc_mu_step pattern 9", {

  # Condition
  val <- c(15, 16)
  ral <- c(15, 20)

  # Run
  mu_step <- calc_mu_step(val, ral)

  # Test
  expect_equal(mu_step, 4)
})

test_that("calc_mu_step pattern 10", {

  # Condition
  val <- c(15, 16, 17)
  ral <- c(15, 16, 18)

  # Run
  mu_step <- calc_mu_step(val, ral)

  # Test
  expect_equal(mu_step, 1)
})

test_that("calc_mu_step pattern 11", {

  # Condition
  val <- c(15, 16, 17)
  ral <- c(15, 17, 18)

  # Run
  mu_step <- calc_mu_step(val, ral)

  # Test
  expect_equal(mu_step, 2)
})

test_that("calc_mu_step pattern 12", {

  # Condition
  val <- c(15, 16, 17)
  ral <- c(15, 18, 19)

  # Run
  mu_step <- calc_mu_step(val, ral)

  # Test
  expect_equal(mu_step, 99)
})

test_that("calc_mu_step pattern 13", {

  # Condition
  val <- c(15, 16, 17)
  ral <- c(15, 16)

  # Run
  mu_step <- calc_mu_step(val, ral)

  # Test
  expect_equal(mu_step, 0)
})

test_that("calc_mu_step pattern 14", {

  # Condition
  val <- c(15, 16, 17)
  ral <- c(15, 17)

  # Run
  mu_step <- calc_mu_step(val, ral)

  # Test
  expect_equal(mu_step, 0)
})

test_that("calc_mu_step pattern 15", {

  # Condition
  val <- c(10, 17)
  ral <- c(15, 17)

  # Run
  mu_step <- calc_mu_step(val, ral)

  # Test
  expect_equal(mu_step, 5)
})

test_that("calc_mu_step pattern 16", {

  # Condition
  val <- numeric(0)
  ral <- 15

  # Run
  mu_step <- calc_mu_step(val, ral)

  # Test
  expect_equal(mu_step, 0)
})

test_that("calc_mu_step pattern 17", {

  # Condition
  val <- 15
  ral <- numeric(0)

  # Run
  mu_step <- calc_mu_step(val, ral)

  # Test
  expect_equal(mu_step, 0)
})

test_that("calc_mu_step pattern 18", {

  # Condition
  val <- numeric(0)
  ral <- c(15, 17)

  # Run
  mu_step <- calc_mu_step(val, ral)

  # Test
  expect_equal(mu_step, 0)
})

