test_that("makeDummyGt 1", {
  vgt <- 11
  rgt <- c(11, 11)
  uniq_vr_al <- sort(unique(c(vgt, rgt)))
  dummy_gt <- make_dummy_gt(vgt, uniq_vr_al)
  expect_equal(length(dummy_gt), 2)
  expect_equal(dummy_gt[[1]][1], 11)
  expect_equal(dummy_gt[[1]][2], 11)
  expect_equal(dummy_gt[[2]][1], 11)
  expect_equal(dummy_gt[[2]][2], 99)
})

test_that("makeDummyGt 2", {
  vgt <- 11
  rgt <- c(11, 12)
  uniq_vr_al <- sort(unique(c(vgt, rgt)))
  dummy_gt <- make_dummy_gt(vgt, uniq_vr_al)
  expect_equal(length(dummy_gt), 3)
  expect_equal(dummy_gt[[1]][1], 11)
  expect_equal(dummy_gt[[1]][2], 11)
  expect_equal(dummy_gt[[2]][1], 11)
  expect_equal(dummy_gt[[2]][2], 12)
  expect_equal(dummy_gt[[3]][1], 11)
  expect_equal(dummy_gt[[3]][2], 99)
})

test_that("makeDummyGt 3", {
  vgt <- 11
  rgt <- c(12, 12)
  uniq_vr_al <- sort(unique(c(vgt, rgt)))
  dummy_gt <- make_dummy_gt(vgt, uniq_vr_al)
  expect_equal(length(dummy_gt), 3)
  expect_equal(dummy_gt[[1]][1], 11)
  expect_equal(dummy_gt[[1]][2], 11)
  expect_equal(dummy_gt[[2]][1], 11)
  expect_equal(dummy_gt[[2]][2], 12)
  expect_equal(dummy_gt[[3]][1], 11)
  expect_equal(dummy_gt[[3]][2], 99)
})

test_that("makeDummyGt 4", {
  vgt <- 11
  rgt <- c(12, 13)
  uniq_vr_al <- sort(unique(c(vgt, rgt)))
  dummy_gt <- make_dummy_gt(vgt, uniq_vr_al)
  expect_equal(length(dummy_gt), 4)
  expect_equal(dummy_gt[[1]][1], 11)
  expect_equal(dummy_gt[[1]][2], 11)
  expect_equal(dummy_gt[[2]][1], 11)
  expect_equal(dummy_gt[[2]][2], 12)
  expect_equal(dummy_gt[[3]][1], 11)
  expect_equal(dummy_gt[[3]][2], 13)
  expect_equal(dummy_gt[[4]][1], 11)
  expect_equal(dummy_gt[[4]][2], 99)
})

test_that("makeDummyGt 5", {
  vgt <- 11
  rgt <- 11
  uniq_vr_al <- sort(unique(c(vgt, rgt)))
  dummy_gt <- make_dummy_gt(vgt, uniq_vr_al)
  expect_equal(length(dummy_gt), 2)
  expect_equal(dummy_gt[[1]][1], 11)
  expect_equal(dummy_gt[[1]][2], 11)
  expect_equal(dummy_gt[[2]][1], 11)
  expect_equal(dummy_gt[[2]][2], 99)
})

test_that("makeDummyGt 6", {
  vgt <- 11
  rgt <- 12
  uniq_vr_al <- sort(unique(c(vgt, rgt)))
  dummy_gt <- make_dummy_gt(vgt, uniq_vr_al)
  expect_equal(length(dummy_gt), 3)
  expect_equal(dummy_gt[[1]][1], 11)
  expect_equal(dummy_gt[[1]][2], 11)
  expect_equal(dummy_gt[[2]][1], 11)
  expect_equal(dummy_gt[[2]][2], 12)
  expect_equal(dummy_gt[[3]][1], 11)
  expect_equal(dummy_gt[[3]][2], 99)
})

