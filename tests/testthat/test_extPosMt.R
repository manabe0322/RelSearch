test_that("extPosMt", {
  range <- "73-340 16024-16365"
  posMt <- extPosMt(range)
  expect_equal(length(posMt), 610)
  expect_equal(posMt[1], 73)
  expect_equal(posMt[268], 340)
  expect_equal(posMt[269], 16024)
  expect_equal(posMt[610], 16365)
})
