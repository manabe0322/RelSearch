test_that("split pattern 1-1", {

  # Run
  al_pre <- split("15", ",")

  # Test
  expect_equal(length(al_pre), 1)
  expect_equal(al_pre[1], "15")
})

test_that("split pattern 1-2", {

  # Run
  al_pre <- split("15", " ")

  # Test
  expect_equal(length(al_pre), 1)
  expect_equal(al_pre[1], "15")
})

test_that("split pattern 2-1", {

  # Run
  al_pre <- split("15,16", ",")

  # Test
  expect_equal(length(al_pre), 2)
  expect_equal(al_pre[1], "15")
  expect_equal(al_pre[2], "16")
})

test_that("split pattern 2-2", {

  # Run
  al_pre <- split("15 16", " ")

  # Test
  expect_equal(length(al_pre), 2)
  expect_equal(al_pre[1], "15")
  expect_equal(al_pre[2], "16")
})

test_that("split pattern 3", {

  # Run
  al_pre <- split("", ",")

  # Test
  expect_equal(al_pre, "")
})
