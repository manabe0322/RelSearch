test_that("makeShareRange", {
  posMtQR <- c(73:340, 16024:16365)
  shareRange <- makeShareRange(posMtQR)
  expect_equal(shareRange, "73-340 16024-16365")
})
