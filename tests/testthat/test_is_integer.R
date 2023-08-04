test_that("is_integer", {
  expect_equal(is_integer(3), TRUE)
  expect_equal(is_integer(3.1), FALSE)
})
