test_that("matchMt pattern 1", {
  qHap <- "16223T 16299G 16362C 73G 263G 315.1C"
  qRan <- "16024-16365 73-340"
  rHap <- "16223T 16299G 16362C 73G 263G 315.1C"
  rRan <- "16024-16365 73-340"
  resultMt <- matchMt(qHap, qRan, rHap, rRan)
  nMis <- resultMt[1]
  shareRange <- resultMt[2]
  lenShare <- resultMt[3]
  expect_equal(nMis, "0")
  expect_equal(shareRange, "73-340 16024-16365")
  expect_equal(lenShare, "610")
})

test_that("matchMt pattern 2", {
  qHap <- "16092C 16209C 16223T 16324C 73G 263G 309.1C 309.2C 315.1C"
  qRan <- "16025-16364 73-340"
  rHap <- "16209C 16223T 16291T 16324C 73G 263G 309.1C 315.1C"
  rRan <- "16024-16365 74-339"
  resultMt <- matchMt(qHap, qRan, rHap, rRan)
  nMis <- resultMt[1]
  shareRange <- resultMt[2]
  lenShare <- resultMt[3]
  expect_equal(nMis, "3")
  expect_equal(shareRange, "74-339 16025-16364")
  expect_equal(lenShare, "606")
})

test_that("matchMt pattern 3", {
  qHap <- "16092C 16209C 16223T 16324C 73G 263G 309.1C 309.2C 315.1C"
  qRan <- "16024-16365"
  rHap <- "16209C 16223T 16291T 16324C 73G 263G 309.1C 315.1C"
  rRan <- "73-340"
  resultMt <- matchMt(qHap, qRan, rHap, rRan)
  nMis <- resultMt[1]
  shareRange <- resultMt[2]
  lenShare <- resultMt[3]
  expect_equal(nMis, "")
  expect_equal(shareRange, "")
  expect_equal(lenShare, "0")
})
