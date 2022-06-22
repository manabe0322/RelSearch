test_that("makeDummyGt 1", {
  qgt <- 11
  rgt <- c(11, 11)
  dummyGt <- makeDummyGt(qgt, rgt)
  expect_equal(nrow(dummyGt), 1)
  expect_equal(dummyGt[1, 1], 11)
  expect_equal(dummyGt[1, 2], 99)
})

test_that("makeDummyGt 2", {
  qgt <- 12
  rgt <- c(11, 11)
  dummyGt <- makeDummyGt(qgt, rgt)
  expect_equal(nrow(dummyGt), 2)
  expect_equal(dummyGt[1, 1], 12)
  expect_equal(dummyGt[1, 2], 11)
  expect_equal(dummyGt[2, 1], 12)
  expect_equal(dummyGt[2, 2], 99)
})

test_that("makeDummyGt 3", {
  qgt <- 11
  rgt <- c(11, 13)
  dummyGt <- makeDummyGt(qgt, rgt)
  expect_equal(nrow(dummyGt), 2)
  expect_equal(dummyGt[1, 1], 11)
  expect_equal(dummyGt[1, 2], 13)
  expect_equal(dummyGt[2, 1], 11)
  expect_equal(dummyGt[2, 2], 99)
})

test_that("makeDummyGt 4", {
  qgt <- 13
  rgt <- c(11, 13)
  dummyGt <- makeDummyGt(qgt, rgt)
  expect_equal(nrow(dummyGt), 2)
  expect_equal(dummyGt[1, 1], 11)
  expect_equal(dummyGt[1, 2], 13)
  expect_equal(dummyGt[2, 1], 13)
  expect_equal(dummyGt[2, 2], 99)
})

test_that("makeDummyGt 5", {
  qgt <- 11
  rgt <- c(12, 13)
  dummyGt <- makeDummyGt(qgt, rgt)
  expect_equal(nrow(dummyGt), 3)
  expect_equal(dummyGt[1, 1], 11)
  expect_equal(dummyGt[1, 2], 12)
  expect_equal(dummyGt[2, 1], 11)
  expect_equal(dummyGt[2, 2], 13)
  expect_equal(dummyGt[3, 1], 11)
  expect_equal(dummyGt[3, 2], 99)
})

