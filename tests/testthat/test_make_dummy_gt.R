test_that("make_dummy_gt 1", {
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

test_that("make_dummy_gt 2", {
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

test_that("make_dummy_gt 3", {
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

test_that("make_dummy_gt 4", {
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

test_that("make_dummy_gt 5", {
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

test_that("make_dummy_gt 6", {
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

test_that("make_dummy_gt 7", {
  vgt <- 12
  rgt <- c(10, 11)
  uniq_vr_al <- sort(unique(c(vgt, rgt)))
  dummy_gt <- make_dummy_gt(vgt, uniq_vr_al)
  expect_equal(length(dummy_gt), 4)
  expect_equal(dummy_gt[[1]][1], 10)
  expect_equal(dummy_gt[[1]][2], 12)
  expect_equal(dummy_gt[[2]][1], 11)
  expect_equal(dummy_gt[[2]][2], 12)
  expect_equal(dummy_gt[[3]][1], 12)
  expect_equal(dummy_gt[[3]][2], 12)
  expect_equal(dummy_gt[[4]][1], 12)
  expect_equal(dummy_gt[[4]][2], 99)
})

test_that("make_dummy_gt 8", {
  vgt <- c(12, 12)
  rgt <- c(10, 11)
  uniq_vr_al <- sort(unique(c(vgt, rgt)))
  dummy_gt <- make_dummy_gt(vgt, uniq_vr_al)
  expect_equal(length(dummy_gt), 1)
  expect_equal(dummy_gt[[1]][1], 12)
  expect_equal(dummy_gt[[1]][2], 12)
})

