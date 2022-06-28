test_that("extPosMtQR pattern 1", {
  qRan <- "73-340 16024-16365"
  rRan <- "73-340 16024-16365"
  posMtQR <- extPosMtQR(qRan, rRan)
  expect_equal(length(posMtQR), 610)
  expect_equal(posMtQR[1], 73)
  expect_equal(posMtQR[268], 340)
  expect_equal(posMtQR[269], 16024)
  expect_equal(posMtQR[610], 16365)
})

test_that("extPosMtQR pattern 2-1", {
  qRan <- "72-339 16024-16365"
  rRan <- "73-340 16025-16364"
  posMtQR <- extPosMtQR(qRan, rRan)
  expect_equal(length(posMtQR), 607)
  expect_equal(posMtQR[1], 73)
  expect_equal(posMtQR[267], 339)
  expect_equal(posMtQR[268], 16025)
  expect_equal(posMtQR[607], 16364)
})

test_that("extPosMtQR pattern 2-2", {
  qRan <- "16024-16365 72-339"
  rRan <- "16025-16364 73-340"
  posMtQR <- extPosMtQR(qRan, rRan)
  expect_equal(length(posMtQR), 607)
  expect_equal(posMtQR[1], 73)
  expect_equal(posMtQR[267], 339)
  expect_equal(posMtQR[268], 16025)
  expect_equal(posMtQR[607], 16364)
})

test_that("extPosMtQR pattern 3", {
  qRan <- "73-340"
  rRan <- "16024-16365"
  posMtQR <- extPosMtQR(qRan, rRan)
  expect_equal(posMtQR, numeric(0))
})

test_that("extPosMtQR pattern 4", {
  qRan <- "16024-16173 16209-16365"
  rRan <- "16050-16250"
  posMtQR <- extPosMtQR(qRan, rRan)
  expect_equal(length(posMtQR), 166)
  expect_equal(posMtQR[1], 16050)
  expect_equal(posMtQR[124], 16173)
  expect_equal(posMtQR[125], 16209)
  expect_equal(posMtQR[166], 16250)
})
