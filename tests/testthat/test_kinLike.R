test_that("kinLike IBS = 0 pattern 1", {
  qgt <- 11
  rgt <- c(12, 12)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  afAl <- 11:15
  probIBD <- c(0.25, 0.5, 0.25)
  consMu <- FALSE
  myu <- 0.002
  ape <- calcApe(af)
  likelihoods <- kinLike(qgt, rgt, af, afAl, probIBD, consMu, myu, ape)
  expect_equal(likelihoods[1], 0.25 * 0.15^2 * 0.25^2)
  expect_equal(likelihoods[2], 0.15^2 * 0.25^2)
})

test_that("kinLike IBS = 0 pattern 2", {
  qgt <- c(11, 13)
  rgt <- c(12, 14)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  afAl <- 11:15
  probIBD <- c(0.25, 0.5, 0.25)
  consMu <- FALSE
  myu <- 0.002
  ape <- calcApe(af)
  likelihoods <- kinLike(qgt, rgt, af, afAl, probIBD, consMu, myu, ape)
  expect_equal(likelihoods[1], 0.25 * (2 * 0.15 * 0.3) * (2 * 0.25 * 0.25))
  expect_equal(likelihoods[2], (2 * 0.15 * 0.3) * (2 * 0.25 * 0.25))
})

test_that("kinLike IBS = 1 pattern 1", {
  qgt <- c(11, 12)
  rgt <- c(11, 13)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  afAl <- 11:15
  probIBD <- c(0.25, 0.5, 0.25)
  consMu <- FALSE
  myu <- 0.002
  ape <- calcApe(af)
  likelihoods <- kinLike(qgt, rgt, af, afAl, probIBD, consMu, myu, ape)
  expect_equal(likelihoods[1], (2 * 0.15 * 0.3) * (0.25 * 0.25 + 0.25 * (2 * 0.15 * 0.25)))
  expect_equal(likelihoods[2], (2 * 0.15 * 0.3) * (2 * 0.15 * 0.25))
})

test_that("kinLike IBS = 1 pattern 2", {
  qgt <- c(11, 12)
  rgt <- c(12, 13)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  afAl <- 11:15
  probIBD <- c(0.25, 0.5, 0.25)
  consMu <- FALSE
  myu <- 0.002
  ape <- calcApe(af)
  likelihoods <- kinLike(qgt, rgt, af, afAl, probIBD, consMu, myu, ape)
  expect_equal(likelihoods[1], (2 * 0.25 * 0.3) * (0.25 * 0.15 + 0.25 * (2 * 0.15 * 0.25)))
  expect_equal(likelihoods[2], (2 * 0.25 * 0.3) * (2 * 0.15 * 0.25))
})

test_that("kinLike IBS = 1 pattern 3", {
  qgt <- c(11, 11)
  rgt <- c(11, 13)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  afAl <- 11:15
  probIBD <- c(0.25, 0.5, 0.25)
  consMu <- FALSE
  myu <- 0.002
  ape <- calcApe(af)
  likelihoods <- kinLike(qgt, rgt, af, afAl, probIBD, consMu, myu, ape)
  expect_equal(likelihoods[1], (2 * 0.15 * 0.3) * (0.25 * 0.15 + 0.25 * (0.15^2)))
  expect_equal(likelihoods[2], (2 * 0.15 * 0.3) * (0.15^2))
})

test_that("kinLike IBS = 1 pattern 4", {
  qgt <- c(13, 13)
  rgt <- c(11, 13)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  afAl <- 11:15
  probIBD <- c(0.25, 0.5, 0.25)
  consMu <- FALSE
  myu <- 0.002
  ape <- calcApe(af)
  likelihoods <- kinLike(qgt, rgt, af, afAl, probIBD, consMu, myu, ape)
  expect_equal(likelihoods[1], (2 * 0.15 * 0.3) * (0.25 * 0.3 + 0.25 * (0.3^2)))
  expect_equal(likelihoods[2], (2 * 0.15 * 0.3) * (0.3^2))
})

test_that("kinLike IBS = 2 pattern 1", {
  qgt <- c(11, 11)
  rgt <- c(11, 11)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  afAl <- 11:15
  probIBD <- c(0.25, 0.5, 0.25)
  consMu <- FALSE
  myu <- 0.002
  ape <- calcApe(af)
  likelihoods <- kinLike(qgt, rgt, af, afAl, probIBD, consMu, myu, ape)
  expect_equal(likelihoods[1], (0.15^2) * (0.25 + 2 * 0.25 * 0.15 + 0.25 * (0.15^2)))
  expect_equal(likelihoods[2], (0.15^2) * (0.15^2))
})

test_that("kinLike IBS = 2 pattern 2", {
  qgt <- c(11, 12)
  rgt <- c(11, 12)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  afAl <- 11:15
  probIBD <- c(0.25, 0.5, 0.25)
  consMu <- FALSE
  myu <- 0.002
  ape <- calcApe(af)
  likelihoods <- kinLike(qgt, rgt, af, afAl, probIBD, consMu, myu, ape)
  expect_equal(likelihoods[1], (2 * 0.15 * 0.25) * (0.25 + 0.25 * (0.15 + 0.25) + 0.25 * (2 * 0.15 * 0.25)))
  expect_equal(likelihoods[2], (2 * 0.15 * 0.25) * (2 * 0.15 * 0.25))
})

test_that("kinLike mutation pattern 1", {
  qgt <- c(11, 12)
  rgt <- c(13, 14)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  afAl <- 11:15
  probIBD <- c(0, 1, 0)
  consMu <- TRUE
  myu <- 0.002
  ape <- calcApe(af)
  likelihoods <- kinLike(qgt, rgt, af, afAl, probIBD, consMu, myu, ape)
  expect_equal(likelihoods[1], 0.002)
  expect_equal(likelihoods[2], 0.352925)
})

test_that("kinLike mutation pattern 2", {
  qgt <- c(11, 12)
  rgt <- c(12, 14)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  afAl <- 11:15
  probIBD <- c(0, 1, 0)
  consMu <- TRUE
  myu <- 0.002
  ape <- calcApe(af)
  likelihoods <- kinLike(qgt, rgt, af, afAl, probIBD, consMu, myu, ape)
  expect_equal(likelihoods[1], 0.5 * (2 * 0.15 * 0.25) * 0.25)
  expect_equal(likelihoods[2], (2 * 0.15 * 0.25) * (2 * 0.25 * 0.25))
})
