test_that("kinLikeDrop 1", {
  qgt <- 11
  rgt <- c(11, 11)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  afAl <- 11:15
  k2 <- 0.25
  k1 <- 0.5 / 2
  k0 <- 0.25
  probIBD <- c(k2, 2 * k1, k0)
  consMu <- FALSE
  myu <- 0.002
  ape <- calcApe(af)
  pd <- 0.5
  likelihoods <- kinLikeDrop(qgt, rgt, af, afAl, probIBD, consMu, myu, ape, pd)

  probRgt <- 0.15^2
  calcHomo_H1 <- (1 - pd) * probRgt * (k2 + 2 * k1 * 0.15 + k0 * 0.15^2)
  calcHetero_H1 <- pd * probRgt * (2 * k1 * 0.85 + k0 * 2 * 0.15 * 0.85)
  calcHomo_H2 <- (1 - pd) * probRgt * 0.15^2
  calcHetero_H2 <- pd * probRgt * 2 * 0.15 * 0.85

  expect_equal(as.numeric(likelihoods[1]), calcHomo_H1 + calcHetero_H1)
  expect_equal(as.numeric(likelihoods[2]), calcHomo_H2 + calcHetero_H2)
})

test_that("kinLikeDrop 2", {
  qgt <- 12
  rgt <- c(11, 11)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  afAl <- 11:15
  k2 <- 0.25
  k1 <- 0.5 / 2
  k0 <- 0.25
  probIBD <- c(k2, 2 * k1, k0)
  consMu <- FALSE
  myu <- 0.002
  ape <- calcApe(af)
  pd <- 0.5
  likelihoods <- kinLikeDrop(qgt, rgt, af, afAl, probIBD, consMu, myu, ape, pd)

  probRgt <- 0.15^2
  calcHomo_H1 <- (1 - pd) * probRgt * k0 * 0.25^2
  calcHetero_H1 <- pd * probRgt * (2 * k1 * 0.25 + k0 * 2 * 0.15 * 0.25 + k0 * 2 * 0.25 * 0.6)
  calcHomo_H2 <- (1 - pd) * probRgt * 0.25^2
  calcHetero_H2 <- pd * probRgt * (2 * 0.15 * 0.25 + 2 * 0.25 * 0.6)

  expect_equal(as.numeric(likelihoods[1]), calcHomo_H1 + calcHetero_H1)
  expect_equal(as.numeric(likelihoods[2]), calcHomo_H2 + calcHetero_H2)
})

test_that("kinLikeDrop 3", {
  qgt <- 11
  rgt <- c(11, 13)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  afAl <- 11:15
  k2 <- 0.25
  k1 <- 0.5 / 2
  k0 <- 0.25
  probIBD <- c(k2, 2 * k1, k0)
  consMu <- FALSE
  myu <- 0.002
  ape <- calcApe(af)
  pd <- 0.5
  likelihoods <- kinLikeDrop(qgt, rgt, af, afAl, probIBD, consMu, myu, ape, pd)

  probRgt <- 2 * 0.15 * 0.3
  calcHomo_H1 <- (1 - pd) * probRgt * (k1 * 0.15 + k0 * 0.15^2)
  calcHetero_H1 <- pd * probRgt * (k2 + k1 * 0.15 + k1 * 0.3 + k0 * 2 * 0.15 * 0.3 + k1 * 0.55 + k0 * 2 * 0.15 * 0.55)
  calcHomo_H2 <- (1 - pd) * probRgt * 0.15^2
  calcHetero_H2 <- pd * probRgt * (2 * 0.15 * 0.3 + 2 * 0.15 * 0.55)

  expect_equal(as.numeric(likelihoods[1]), calcHomo_H1 + calcHetero_H1)
  expect_equal(as.numeric(likelihoods[2]), calcHomo_H2 + calcHetero_H2)
})

test_that("kinLikeDrop 4", {
  qgt <- 13
  rgt <- c(11, 13)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  afAl <- 11:15
  k2 <- 0.25
  k1 <- 0.5 / 2
  k0 <- 0.25
  probIBD <- c(k2, 2 * k1, k0)
  consMu <- FALSE
  myu <- 0.002
  ape <- calcApe(af)
  pd <- 0.5
  likelihoods <- kinLikeDrop(qgt, rgt, af, afAl, probIBD, consMu, myu, ape, pd)

  probRgt <- 2 * 0.15 * 0.3
  calcHomo_H1 <- (1 - pd) * probRgt * (k1 * 0.3 + k0 * 0.3^2)
  calcHetero_H1 <- pd * probRgt * (k2 + k1 * 0.15 + k1 * 0.3 + k0 * 2 * 0.15 * 0.3 + k1 * 0.55 + k0 * 2 * 0.3 * 0.55)
  calcHomo_H2 <- (1 - pd) * probRgt * 0.3^2
  calcHetero_H2 <- pd * probRgt * (2 * 0.15 * 0.3 + 2 * 0.3 * 0.55)

  expect_equal(as.numeric(likelihoods[1]), calcHomo_H1 + calcHetero_H1)
  expect_equal(as.numeric(likelihoods[2]), calcHomo_H2 + calcHetero_H2)
})

test_that("kinLikeDrop 5", {
  qgt <- 11
  rgt <- c(12, 13)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  afAl <- 11:15
  k2 <- 0.25
  k1 <- 0.5 / 2
  k0 <- 0.25
  probIBD <- c(k2, 2 * k1, k0)
  consMu <- FALSE
  myu <- 0.002
  ape <- calcApe(af)
  pd <- 0.5
  likelihoods <- kinLikeDrop(qgt, rgt, af, afAl, probIBD, consMu, myu, ape, pd)

  probRgt <- 2 * 0.25 * 0.3
  calcHomo_H1 <- (1 - pd) * probRgt * (k0 * 0.15^2)
  calcHetero_H1 <- pd * probRgt * (k1 * 0.15 + k0 * 2 * 0.15 * 0.25 + k1 * 0.15 + k0 * 2 * 0.15 * 0.3 + k0 * 2 * 0.15 * 0.3)
  calcHomo_H2 <- (1 - pd) * probRgt * 0.15^2
  calcHetero_H2 <- pd * probRgt * (2 * 0.15 * 0.25 + 2 * 0.15 * 0.3 + 2 * 0.15 * 0.3)

  expect_equal(as.numeric(likelihoods[1]), calcHomo_H1 + calcHetero_H1)
  expect_equal(as.numeric(likelihoods[2]), calcHomo_H2 + calcHetero_H2)
})

test_that("kinLikeDrop mutation pattern 1 (actually not used)", {
  qgt <- 11
  rgt <- c(12, 12)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  afAl <- 11:15
  probIBD <- c(0, 1, 0)
  consMu <- TRUE
  myu <- 0.002
  ape <- calcApe(af)
  pd <- 0.5
  likelihoods <- kinLikeDrop(qgt, rgt, af, afAl, probIBD, consMu, myu, ape, pd)

  probRgt <- 0.25^2
  calcHomo_H1 <- (1 - pd) * myu
  calcHetero_H1 <- pd * (probRgt * 0.15 + myu)
  calcHomo_H2 <- (1 - pd) * ape
  calcHetero_H2 <- pd * (probRgt * 2 * 0.15 * 0.25 + ape)

  expect_equal(as.numeric(likelihoods[1]), calcHomo_H1 + calcHetero_H1)
  expect_equal(as.numeric(likelihoods[2]), calcHomo_H2 + calcHetero_H2)
})

test_that("kinLikeDrop mutation pattern 2", {
  qgt <- 12
  rgt <- c(11, 13)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  afAl <- 11:15
  probIBD <- c(0, 1, 0)
  consMu <- FALSE
  myu <- 0.002
  ape <- calcApe(af)
  pd <- 0.5
  likelihoods <- kinLikeDrop(qgt, rgt, af, afAl, probIBD, consMu, myu, ape, pd)

  probRgt <- 2 * 0.15 * 0.3
  calcHomo_H1 <- 0
  calcHetero_H1 <- pd * probRgt * (0.5 * 0.25 + 0.5 * 0.25)
  calcHomo_H2 <- (1 - pd) * probRgt * 0.25^2
  calcHetero_H2 <- pd * probRgt * (2 * 0.15 * 0.25 + 2 * 0.25 * 0.3 + 2 * 0.25 * 0.3)

  expect_equal(as.numeric(likelihoods[1]), calcHomo_H1 + calcHetero_H1)
  expect_equal(as.numeric(likelihoods[2]), calcHomo_H2 + calcHetero_H2)
})

