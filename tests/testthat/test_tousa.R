test_that("tousa pattern 1", {
  vec <- tousa(3, 12, 3)
  expect_equal(length(vec), 4)
  expect_equal(vec[1], 3)
  expect_equal(vec[2], 6)
  expect_equal(vec[3], 9)
  expect_equal(vec[4], 12)
})

test_that("tousa pattern 2", {
  vec <- tousa(3, 11, 2)
  expect_equal(length(vec), 5)
  expect_equal(vec[1], 3)
  expect_equal(vec[2], 5)
  expect_equal(vec[3], 7)
  expect_equal(vec[4], 9)
  expect_equal(vec[5], 11)
})

test_that("tousa pattern 3", {
  vec <- tousa(3, 3, 1)
  expect_equal(length(vec), 1)
  expect_equal(vec[1], 3)
})
