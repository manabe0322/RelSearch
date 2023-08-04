test_that("extract_integer", {
  expect_equal(extract_integer("73G"), 73)
  expect_equal(extract_integer("249-"), 249)
  expect_equal(extract_integer("249del"), 249)
  expect_equal(extract_integer("309.1C"), 309)
})
