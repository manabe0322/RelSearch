test_that("search_pos_double", {

  # Condition
  vec <- c(1.1, 1.2, 1.3, 1.4, 1.5)

  # Test
  expect_equal(search_pos_double(vec, 1.1), 0)
  expect_equal(search_pos_double(vec, 1.2), 1)
  expect_equal(search_pos_double(vec, 1.3), 2)
  expect_equal(search_pos_double(vec, 1.4), 3)
  expect_equal(search_pos_double(vec, 1.5), 4)
  expect_equal(search_pos_double(vec, 1.6), 5)
})
