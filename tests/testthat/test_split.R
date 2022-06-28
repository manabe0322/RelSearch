test_that("split pattern 1-1", {
  alPre <- split("15", ",")
  expect_equal(length(alPre), 1)
  expect_equal(alPre[1], "15")
})

test_that("split pattern 1-2", {
  alPre <- split("15", " ")
  expect_equal(length(alPre), 1)
  expect_equal(alPre[1], "15")
})

test_that("split pattern 2-1", {
  alPre <- split("15,16", ",")
  expect_equal(length(alPre), 2)
  expect_equal(alPre[1], "15")
  expect_equal(alPre[2], "16")
})

test_that("split pattern 2-2", {
  alPre <- split("15, 16", ",")
  expect_equal(length(alPre), 2)
  expect_equal(alPre[1], "15")
  expect_equal(alPre[2], " 16")
})

test_that("split pattern 2-3", {
  alPre <- split("15 16", " ")
  expect_equal(length(alPre), 2)
  expect_equal(alPre[1], "15")
  expect_equal(alPre[2], "16")
})

test_that("split pattern 3", {
  alPre <- split("", ",")
  expect_equal(alPre, character(0))
})
