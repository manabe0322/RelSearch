test_that("calcApe", {
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  expect_equal(calcApe(af), 0.352925)
})
