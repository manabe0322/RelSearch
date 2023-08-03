test_that("calc_kin_like IBS = 0 pattern 1", {
  vgt <- 11
  rgt <- c(12, 12)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  af_al <- 11:15
  pibd <- c(0.25, 0.5, 0.25)
  cons_mu <- FALSE
  myu <- 0.002
  ape <- calc_ape(af)
  likelihoods <- calc_kin_like(vgt, rgt, af, af_al, pibd, cons_mu, myu, ape)
  expect_equal(likelihoods[1], 0.25 * 0.15^2 * 0.25^2)
  expect_equal(likelihoods[2], 0.15^2 * 0.25^2)
})

test_that("calc_kin_like IBS = 0 pattern 2", {
  vgt <- c(11, 13)
  rgt <- c(12, 14)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  af_al <- 11:15
  pibd <- c(0.25, 0.5, 0.25)
  cons_mu <- FALSE
  myu <- 0.002
  ape <- calc_ape(af)
  likelihoods <- calc_kin_like(vgt, rgt, af, af_al, pibd, cons_mu, myu, ape)
  expect_equal(likelihoods[1], 0.25 * (2 * 0.15 * 0.3) * (2 * 0.25 * 0.25))
  expect_equal(likelihoods[2], (2 * 0.15 * 0.3) * (2 * 0.25 * 0.25))
})

test_that("calc_kin_like IBS = 1 pattern 1", {
  vgt <- c(11, 12)
  rgt <- c(11, 13)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  af_al <- 11:15
  pibd <- c(0.25, 0.5, 0.25)
  cons_mu <- FALSE
  myu <- 0.002
  ape <- calc_ape(af)
  likelihoods <- calc_kin_like(vgt, rgt, af, af_al, pibd, cons_mu, myu, ape)
  expect_equal(likelihoods[1], (2 * 0.15 * 0.3) * (0.25 * 0.25 + 0.25 * (2 * 0.15 * 0.25)))
  expect_equal(likelihoods[2], (2 * 0.15 * 0.3) * (2 * 0.15 * 0.25))
})

test_that("calc_kin_like IBS = 1 pattern 2", {
  vgt <- c(11, 12)
  rgt <- c(12, 13)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  af_al <- 11:15
  pibd <- c(0.25, 0.5, 0.25)
  cons_mu <- FALSE
  myu <- 0.002
  ape <- calc_ape(af)
  likelihoods <- calc_kin_like(vgt, rgt, af, af_al, pibd, cons_mu, myu, ape)
  expect_equal(likelihoods[1], (2 * 0.25 * 0.3) * (0.25 * 0.15 + 0.25 * (2 * 0.15 * 0.25)))
  expect_equal(likelihoods[2], (2 * 0.25 * 0.3) * (2 * 0.15 * 0.25))
})

test_that("calc_kin_like IBS = 1 pattern 3", {
  vgt <- c(11, 11)
  rgt <- c(11, 13)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  af_al <- 11:15
  pibd <- c(0.25, 0.5, 0.25)
  cons_mu <- FALSE
  myu <- 0.002
  ape <- calc_ape(af)
  likelihoods <- calc_kin_like(vgt, rgt, af, af_al, pibd, cons_mu, myu, ape)
  expect_equal(likelihoods[1], (2 * 0.15 * 0.3) * (0.25 * 0.15 + 0.25 * (0.15^2)))
  expect_equal(likelihoods[2], (2 * 0.15 * 0.3) * (0.15^2))
})

test_that("calc_kin_like IBS = 1 pattern 4", {
  vgt <- c(13, 13)
  rgt <- c(11, 13)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  af_al <- 11:15
  pibd <- c(0.25, 0.5, 0.25)
  cons_mu <- FALSE
  myu <- 0.002
  ape <- calc_ape(af)
  likelihoods <- calc_kin_like(vgt, rgt, af, af_al, pibd, cons_mu, myu, ape)
  expect_equal(likelihoods[1], (2 * 0.15 * 0.3) * (0.25 * 0.3 + 0.25 * (0.3^2)))
  expect_equal(likelihoods[2], (2 * 0.15 * 0.3) * (0.3^2))
})

test_that("calc_kin_like IBS = 1 pattern 5", {
  vgt <- c(11, 99)
  rgt <- c(11, 11)
  af <- c(0.15, 0.85)
  af_al <- c(11, 99)
  pibd <- c(0.25, 0.5, 0.25)
  cons_mu <- FALSE
  myu <- 0.002
  ape <- calc_ape(af)
  likelihoods <- calc_kin_like(vgt, rgt, af, af_al, pibd, cons_mu, myu, ape)
  expect_equal(likelihoods[1], 0.15^2 * (2 * 0.25 * 0.85 + 0.25 * (2 * 0.15 * 0.85)))
  expect_equal(likelihoods[2], 0.15^2 * (2 * 0.15 * 0.85))
})

test_that("calc_kin_like IBS = 2 pattern 1", {
  vgt <- c(11, 11)
  rgt <- c(11, 11)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  af_al <- 11:15
  pibd <- c(0.25, 0.5, 0.25)
  cons_mu <- FALSE
  myu <- 0.002
  ape <- calc_ape(af)
  likelihoods <- calc_kin_like(vgt, rgt, af, af_al, pibd, cons_mu, myu, ape)
  expect_equal(likelihoods[1], (0.15^2) * (0.25 + 2 * 0.25 * 0.15 + 0.25 * (0.15^2)))
  expect_equal(likelihoods[2], (0.15^2) * (0.15^2))
})

test_that("calc_kin_like IBS = 2 pattern 2", {
  vgt <- c(11, 12)
  rgt <- c(11, 12)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  af_al <- 11:15
  pibd <- c(0.25, 0.5, 0.25)
  cons_mu <- FALSE
  myu <- 0.002
  ape <- calc_ape(af)
  likelihoods <- calc_kin_like(vgt, rgt, af, af_al, pibd, cons_mu, myu, ape)
  expect_equal(likelihoods[1], (2 * 0.15 * 0.25) * (0.25 + 0.25 * (0.15 + 0.25) + 0.25 * (2 * 0.15 * 0.25)))
  expect_equal(likelihoods[2], (2 * 0.15 * 0.25) * (2 * 0.15 * 0.25))
})

test_that("calc_kin_like mutation pattern 1", {
  vgt <- c(11, 12)
  rgt <- c(13, 14)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  af_al <- 11:15
  pibd <- c(0, 1, 0)
  cons_mu <- TRUE
  myu <- 0.002
  ape <- calc_ape(af)
  likelihoods <- calc_kin_like(vgt, rgt, af, af_al, pibd, cons_mu, myu, ape)
  expect_equal(likelihoods[1], 0.002)
  expect_equal(likelihoods[2], 0.352925)
})

test_that("calc_kin_like mutation pattern 2", {
  vgt <- c(11, 12)
  rgt <- c(12, 14)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  af_al <- 11:15
  pibd <- c(0, 1, 0)
  cons_mu <- TRUE
  myu <- 0.002
  ape <- calc_ape(af)
  likelihoods <- calc_kin_like(vgt, rgt, af, af_al, pibd, cons_mu, myu, ape)
  expect_equal(likelihoods[1], 0.5 * (2 * 0.15 * 0.25) * 0.25)
  expect_equal(likelihoods[2], (2 * 0.15 * 0.25) * (2 * 0.25 * 0.25))
})
