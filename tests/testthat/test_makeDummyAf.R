test_that("makeDummyAf 1", {
  dummyGt <- matrix(c(11, 99), ncol = 2, byrow = TRUE)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  afAl <- 11:15
  dummyData <- makeDummyAf(dummyGt, af, afAl)
  af_dummy <- dummyData[1, ]
  afAl_dummy <- dummyData[2, ]
  expect_equal(length(af_dummy), 2)
  expect_equal(af_dummy[1], 0.15)
  expect_equal(af_dummy[2], 0.85)
  expect_equal(length(afAl_dummy), 2)
  expect_equal(afAl_dummy[1], 11)
  expect_equal(afAl_dummy[2], 99)
})

test_that("makeDummyAf 2", {
  dummyGt <- matrix(c(12, 11, 12, 99), ncol = 2, byrow = TRUE)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  afAl <- 11:15
  dummyData <- makeDummyAf(dummyGt, af, afAl)
  af_dummy <- dummyData[1, ]
  afAl_dummy <- dummyData[2, ]
  expect_equal(length(af_dummy), 3)
  expect_equal(af_dummy[1], 0.15)
  expect_equal(af_dummy[2], 0.25)
  expect_equal(af_dummy[3], 0.6)
  expect_equal(length(afAl_dummy), 3)
  expect_equal(afAl_dummy[1], 11)
  expect_equal(afAl_dummy[2], 12)
  expect_equal(afAl_dummy[3], 99)
})

test_that("makeDummyAf 3", {
  dummyGt <- matrix(c(11, 13, 11, 99), ncol = 2, byrow = TRUE)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  afAl <- 11:15
  dummyData <- makeDummyAf(dummyGt, af, afAl)
  af_dummy <- dummyData[1, ]
  afAl_dummy <- dummyData[2, ]
  expect_equal(length(af_dummy), 3)
  expect_equal(af_dummy[1], 0.15)
  expect_equal(af_dummy[2], 0.3)
  expect_equal(af_dummy[3], 0.55)
  expect_equal(length(afAl_dummy), 3)
  expect_equal(afAl_dummy[1], 11)
  expect_equal(afAl_dummy[2], 13)
  expect_equal(afAl_dummy[3], 99)
})

test_that("makeDummyAf 4", {
  dummyGt <- matrix(c(11, 13, 13, 99), ncol = 2, byrow = TRUE)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  afAl <- 11:15
  dummyData <- makeDummyAf(dummyGt, af, afAl)
  af_dummy <- dummyData[1, ]
  afAl_dummy <- dummyData[2, ]
  expect_equal(length(af_dummy), 3)
  expect_equal(af_dummy[1], 0.15)
  expect_equal(af_dummy[2], 0.3)
  expect_equal(af_dummy[3], 0.55)
  expect_equal(length(afAl_dummy), 3)
  expect_equal(afAl_dummy[1], 11)
  expect_equal(afAl_dummy[2], 13)
  expect_equal(afAl_dummy[3], 99)
})

test_that("makeDummyAf 5", {
  dummyGt <- matrix(c(11, 12, 11, 13, 11, 99), ncol = 2, byrow = TRUE)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  afAl <- 11:15
  dummyData <- makeDummyAf(dummyGt, af, afAl)
  af_dummy <- dummyData[1, ]
  afAl_dummy <- dummyData[2, ]
  expect_equal(length(af_dummy), 4)
  expect_equal(af_dummy[1], 0.15)
  expect_equal(af_dummy[2], 0.25)
  expect_equal(af_dummy[3], 0.3)
  expect_equal(af_dummy[3], 0.3)
  expect_equal(length(afAl_dummy), 4)
  expect_equal(afAl_dummy[1], 11)
  expect_equal(afAl_dummy[2], 12)
  expect_equal(afAl_dummy[3], 13)
  expect_equal(afAl_dummy[4], 99)
})

