test_that("count_both_not_na_auto, pattern 1", {
  # Condition
  vec1 <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  vec2 <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)

  # Run
  count <- count_both_not_na_auto(vec1, vec2)

  # Test
  expect_equal(count, 6)
})

test_that("count_both_not_na_auto, pattern 2", {
  # Condition
  vec1 <- c(1, 2, 3, 4, -99, 6, -99, -99, 9, 10, -99, -99)
  vec2 <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)

  # Run
  count <- count_both_not_na_auto(vec1, vec2)

  # Test
  expect_equal(count, 4)
})

test_that("count_both_not_na_auto, pattern 3", {
  # Condition
  vec1 <- c(1, 2, 3, 4, -99, 6, -99, -99, 9, 10, -99, -99)
  vec2 <- c(1, 2, -99, 4, 5, 6, 7, 8, -99, -99, -99, -99)

  # Run
  count <- count_both_not_na_auto(vec1, vec2)

  # Test
  expect_equal(count, 3)
})
