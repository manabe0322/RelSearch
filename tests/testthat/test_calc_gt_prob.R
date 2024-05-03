test_that("calc_gt_prob pattern1", {
  # Condition
  gt <- c(11, 11)
  al_prob_1 <- 0.3
  al_prob_2 <- 0.3

  # Run
  gt_prob <- calc_gt_prob(gt, al_prob_1, al_prob_2)

  # Test
  expect_equal(gt_prob, 0.09)
})

test_that("calc_gt_prob pattern2", {
  # Condition
  gt <- c(11, 12)
  al_prob_1 <- 0.3
  al_prob_2 <- 0.2

  # Run
  gt_prob <- calc_gt_prob(gt, al_prob_1, al_prob_2)

  # Test
  expect_equal(gt_prob, 0.12)
})
