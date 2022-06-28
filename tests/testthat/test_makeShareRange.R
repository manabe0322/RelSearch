test_that("makeShareRange pattern 1", {
  posMtQR <- c(73:340, 16024:16365)
  shareRange <- makeShareRange(posMtQR)
  expect_equal(shareRange, "73-340 16024-16365")
})

test_that("makeShareRange pattern 2", {
  posMtQR <- c(340, 16024:16365)
  shareRange <- makeShareRange(posMtQR)
  expect_equal(shareRange, "340 16024-16365")
})

test_that("makeShareRange pattern 3", {
  qRan <- "72-339 16024-16365"
  rRan <- "73-340 16025-16364"
  posMtQR <- extPosMtQR(qRan, rRan)
  shareRange <- makeShareRange(posMtQR)
  expect_equal(shareRange, "73-339 16025-16364")
})
