test_that("calcApe", {
  afOneL <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  expect_equal(calcApe(afOneL), 0.352925)
})
