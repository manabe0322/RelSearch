test_that("search_pos_int", {

  # Condition
  vec <- c(2, 4, 6, 8, 10)

  # Test
  expect_equal(search_pos_int(vec, 2), 0)
  expect_equal(search_pos_int(vec, 4), 1)
  expect_equal(search_pos_int(vec, 6), 2)
  expect_equal(search_pos_int(vec, 8), 3)
  expect_equal(search_pos_int(vec, 10), 4)
  expect_equal(search_pos_int(vec, 12), 5)
})
