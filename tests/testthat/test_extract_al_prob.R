test_that("extract_al_prob", {
  # Condition
  gt1 <- c(11, 13)
  gt2 <- c(12, 99)
  af_dummy <- c(0.3, 0.2, 0.1, 0.05, 0.35)
  af_al_dummy <- c(11, 12, 13, 14, 99)

  # Run
  al_prob <- extract_al_prob(gt1, gt2, af_dummy, af_al_dummy)

  # Test
  expect_equal(length(al_prob), 4)
  expect_equal(al_prob[1], 0.3)
  expect_equal(al_prob[2], 0.1)
  expect_equal(al_prob[3], 0.2)
  expect_equal(al_prob[4], 0.35)
})
