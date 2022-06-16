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
