test_that("is_integer", {
  expect_equal(is_integer(3), TRUE)
  expect_equal(is_integer(3.1), FALSE)
  expect_equal(is_integer(10 - 8), TRUE)
  expect_equal(is_integer(10.2 - 8.2), TRUE)
  expect_equal(is_integer(10 - 8.2), FALSE)
})
