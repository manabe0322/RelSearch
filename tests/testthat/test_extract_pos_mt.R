test_that("extract_pos_mt pattern 1", {

  # Condition
  range <- "73-340 16024-16365"

  # Run
  pos_mt <- extract_pos_mt(range)

  # Test
  expect_equal(length(pos_mt), 610)
  expect_equal(pos_mt[1], 73)
  expect_equal(pos_mt[268], 340)
  expect_equal(pos_mt[269], 16024)
  expect_equal(pos_mt[610], 16365)
})

test_that("extract_pos_mt pattern 2", {

  # Condition
  range <- "340 16024-16365"

  # Run
  pos_mt <- extract_pos_mt(range)

  # Test
  expect_equal(length(pos_mt), 343)
  expect_equal(pos_mt[1], 340)
  expect_equal(pos_mt[2], 16024)
  expect_equal(pos_mt[343], 16365)
})
