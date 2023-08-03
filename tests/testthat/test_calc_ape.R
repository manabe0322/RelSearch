test_that("calc_ape", {

  # Condition
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)

  # Run
  ape <- calc_ape(af)

  # Test
  expect_equal(ape, 0.352925)
})
