test_that("correct_af_dirichlet, pattern1", {
  # Condition
  pop_al <- 12:21
  pop_freq <- c(6, 3, 79, 1192, 910, 608, 194, 8, 1, 1)
  unobs_al <- numeric(0)

  # Run
  tmp <- correct_af_dirichlet(pop_al, pop_freq, unobs_al)
  pop_al <- tmp$pop_al
  pop_prob <- tmp$pop_prob

  # Test
  expect_equal(length(pop_al), 10)
  expect_equal(length(pop_prob), 10)
  expect_equal(pop_al[1], 12)
  expect_equal(pop_al[2], 13)
  expect_equal(pop_al[3], 14)
  expect_equal(pop_al[4], 15)
  expect_equal(pop_al[5], 16)
  expect_equal(pop_al[6], 17)
  expect_equal(pop_al[7], 18)
  expect_equal(pop_al[8], 19)
  expect_equal(pop_al[9], 20)
  expect_equal(pop_al[10], 21)
  expect_equal(round(pop_prob[1], 9), 0.002324037)
  expect_equal(round(pop_prob[2], 9), 0.001328021)
  expect_equal(round(pop_prob[3], 9), 0.026560425)
  expect_equal(round(pop_prob[4], 9), 0.396082337)
  expect_equal(round(pop_prob[5], 9), 0.302456839)
  expect_equal(round(pop_prob[6], 9), 0.202191235)
  expect_equal(round(pop_prob[7], 9), 0.064741036)
  expect_equal(round(pop_prob[8], 9), 0.002988048)
  expect_equal(round(pop_prob[9], 9), 0.000664011)
  expect_equal(round(pop_prob[10], 9), 0.000664011)
})

test_that("correct_af_dirichlet, pattern2", {
  # Condition
  pop_al <- 12:20
  pop_freq <- c(6, 3, 79, 1192, 910, 608, 194, 8, 2)
  unobs_al <- 22

  # Run
  tmp <- correct_af_dirichlet(pop_al, pop_freq, unobs_al)
  pop_al <- tmp$pop_al
  pop_prob <- tmp$pop_prob

  # Test
  expect_equal(length(pop_al), 10)
  expect_equal(length(pop_prob), 10)
  expect_equal(pop_al[1], 12)
  expect_equal(pop_al[2], 13)
  expect_equal(pop_al[3], 14)
  expect_equal(pop_al[4], 15)
  expect_equal(pop_al[5], 16)
  expect_equal(pop_al[6], 17)
  expect_equal(pop_al[7], 18)
  expect_equal(pop_al[8], 19)
  expect_equal(pop_al[9], 20)
  expect_equal(pop_al[10], 22)
  expect_equal(round(pop_prob[1], 9), round(7 / 3012, 9))
  expect_equal(round(pop_prob[2], 9), round(4 / 3012, 9))
  expect_equal(round(pop_prob[3], 9), round(80 / 3012, 9))
  expect_equal(round(pop_prob[4], 9), round(1193 / 3012, 9))
  expect_equal(round(pop_prob[5], 9), round(911 / 3012, 9))
  expect_equal(round(pop_prob[6], 9), round(609 / 3012, 9))
  expect_equal(round(pop_prob[7], 9), round(195 / 3012, 9))
  expect_equal(round(pop_prob[8], 9), round(9 / 3012, 9))
  expect_equal(round(pop_prob[9], 9), round(3 / 3012, 9))
  expect_equal(round(pop_prob[10], 9), round(1 / 3012, 9))
})
