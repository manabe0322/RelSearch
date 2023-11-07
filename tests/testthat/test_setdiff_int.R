test_that("setdiff_int pattern 1", {
  # Condition
  vec_int1 <- 1:10
  vec_int2 <- 1:10

  # Run
  ans1 <- setdiff_int(vec_int1, vec_int2)
  ans2 <- setdiff_int(vec_int2, vec_int1)

  # Test
  expect_equal(ans1, integer(0))
  expect_equal(ans2, integer(0))
})

test_that("setdiff_int pattern 2", {
  # Condition
  vec_int1 <- 1:10
  vec_int2 <- c(2, 4, 6, 8, 10)

  # Run
  ans1 <- setdiff_int(vec_int1, vec_int2)
  ans2 <- setdiff_int(vec_int2, vec_int1)

  # Test
  expect_equal(length(ans1), 5)
  expect_equal(ans1[1], 1)
  expect_equal(ans1[2], 3)
  expect_equal(ans1[3], 5)
  expect_equal(ans1[4], 7)
  expect_equal(ans1[5], 9)
  expect_equal(ans2, integer(0))
})

test_that("setdiff_int pattern 3", {
  # Condition
  vec_int1 <- c(1, 2, 3, 4, 5)
  vec_int2 <- c(2, 4, 6, 8, 10)

  # Run
  ans1 <- setdiff_int(vec_int1, vec_int2)
  ans2 <- setdiff_int(vec_int2, vec_int1)

  # Test
  expect_equal(length(ans1), 3)
  expect_equal(ans1[1], 1)
  expect_equal(ans1[2], 3)
  expect_equal(ans1[3], 5)
  expect_equal(length(ans2), 3)
  expect_equal(ans2[1], 6)
  expect_equal(ans2[2], 8)
  expect_equal(ans2[3], 10)
})

test_that("setdiff_int pattern 4", {
  # Condition
  vec_int1 <- c(3, 4, 5)
  vec_int2 <- integer(0)

  # Run
  ans1 <- setdiff_int(vec_int1, vec_int2)
  ans2 <- setdiff_int(vec_int2, vec_int1)

  # Test
  expect_equal(length(ans1), 3)
  expect_equal(ans1[1], 3)
  expect_equal(ans1[2], 4)
  expect_equal(ans1[3], 5)
  expect_equal(ans2, integer(0))
})
