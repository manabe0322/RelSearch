test_that("calcMuStep pattern 1", {
  qAl <- 15
  rAl <- 16
  muStep <- calcMuStep(qAl, rAl)
  expect_equal(muStep, 1)
})

test_that("calcMuStep pattern 2", {
  qAl <- 15
  rAl <- c(17, 18)
  muStep <- calcMuStep(qAl, rAl)
  expect_equal(muStep, 2)
})

test_that("calcMuStep pattern 3", {
  qAl <- 15
  rAl <- c(15, 17)
  muStep <- calcMuStep(qAl, rAl)
  expect_equal(muStep, 2)
})

test_that("calcMuStep pattern 4", {
  qAl <- 15
  rAl <- 15
  muStep <- calcMuStep(qAl, rAl)
  expect_equal(muStep, 0)
})

test_that("calcMuStep pattern 5", {
  qAl <- 15
  rAl <- 15.1
  muStep <- calcMuStep(qAl, rAl)
  expect_equal(muStep, 99)
})

test_that("calcMuStep pattern 6", {
  qAl <- 15
  rAl <- c(15.1, 16)
  muStep <- calcMuStep(qAl, rAl)
  expect_equal(muStep, 1)
})
