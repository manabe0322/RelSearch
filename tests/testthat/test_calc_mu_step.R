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
  expect_equal(mu_step, 2)
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
