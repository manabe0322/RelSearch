test_that("set_prob_drop_gt, homozygote, designated two alleles", {

  # Condition
  gt <- c(11, 11)
  uniq_al <- c(11, 12, 13)
  dummy_gt <- make_dummy_gt(gt, uniq_al)
  pd <- 0.3

  # Run
  prob_drop_gt <- set_prob_drop_gt(gt, dummy_gt, pd)

  # Test
  expect_equal(length(prob_drop_gt), 1)
  expect_equal(prob_drop_gt[1], 0.49)
})

test_that("set_prob_drop_gt, heterozygote, designated two alleles", {

  # Condition
  gt <- c(11, 12)
  uniq_al <- c(11, 12, 13)
  dummy_gt <- make_dummy_gt(gt, uniq_al)
  pd <- 0.3

  # Run
  prob_drop_gt <- set_prob_drop_gt(gt, dummy_gt, pd)

  # Test
  expect_equal(length(prob_drop_gt), 1)
  expect_equal(prob_drop_gt[1], 0.49)
})

test_that("set_prob_drop_gt, designated one allele", {

  # Condition
  gt <- c(11)
  uniq_al <- c(11, 12, 13)
  dummy_gt <- make_dummy_gt(gt, uniq_al)
  pd <- 0.3

  # Run
  prob_drop_gt <- set_prob_drop_gt(gt, dummy_gt, pd)

  # Test
  expect_equal(length(prob_drop_gt), 4)
  expect_equal(prob_drop_gt[1], 0.49)
  expect_equal(prob_drop_gt[2], 0.21)
  expect_equal(prob_drop_gt[3], 0.21)
  expect_equal(prob_drop_gt[4], 0.21)
})

