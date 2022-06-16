test_that("kinLike IBS = 0 pattern 1", {
  qgt <- 11
  rgt <- c(12, 12)
  afOneL <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  afAlOneL <- 11:15
  probIBD <- c(0.25, 0.5, 0.25)
  likelihoods <- kinLike(qgt, rgt, afOneL, afAlOneL, probIBD)
  expect_equal(likelihoods[1], 0.25 * 0.15^2 * 0.25^2)
  expect_equal(likelihoods[2], 0.15^2 * 0.25^2)
})

test_that("kinLike IBS = 0 pattern 2", {
  qgt <- c(11, 13)
  rgt <- c(12, 14)
  afOneL <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  afAlOneL <- 11:15
  probIBD <- c(0.25, 0.5, 0.25)
  likelihoods <- kinLike(qgt, rgt, afOneL, afAlOneL, probIBD)
  expect_equal(likelihoods[1], 0.25 * (2 * 0.15 * 0.3) * (2 * 0.25 * 0.25))
  expect_equal(likelihoods[2], (2 * 0.15 * 0.3) * (2 * 0.25 * 0.25))
})

test_that("kinLike IBS = 1 pattern 1", {
  qgt <- c(11, 12)
  rgt <- c(11, 13)
  afOneL <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  afAlOneL <- 11:15
  probIBD <- c(0.25, 0.5, 0.25)
  likelihoods <- kinLike(qgt, rgt, afOneL, afAlOneL, probIBD)
  expect_equal(likelihoods[1], 0.25 * (2 * 0.15 * 0.25) * 0.3 + 0.25 * (2 * 0.15 * 0.25) * (2 * 0.15 * 0.3))
  expect_equal(likelihoods[2], (2 * 0.15 * 0.25) * (2 * 0.15 * 0.3))
})

test_that("kinLike IBS = 1 pattern 2", {
  qgt <- c(11, 12)
  rgt <- c(12, 13)
  afOneL <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  afAlOneL <- 11:15
  probIBD <- c(0.25, 0.5, 0.25)
  likelihoods <- kinLike(qgt, rgt, afOneL, afAlOneL, probIBD)
  expect_equal(likelihoods[1], 0.25 * (2 * 0.15 * 0.25) * 0.3 + 0.25 * (2 * 0.15 * 0.25) * (2 * 0.25 * 0.3))
  expect_equal(likelihoods[2], (2 * 0.15 * 0.25) * (2 * 0.25 * 0.3))
})

test_that("kinLike IBS = 1 pattern 3", {
  qgt <- c(11, 11)
  rgt <- c(11, 13)
  afOneL <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  afAlOneL <- 11:15
  probIBD <- c(0.25, 0.5, 0.25)
  likelihoods <- kinLike(qgt, rgt, afOneL, afAlOneL, probIBD)
  expect_equal(likelihoods[1], 2 * 0.25 * (0.15^2) * 0.3 + 0.25 * (0.15^2) * (2 * 0.15 * 0.3))
  expect_equal(likelihoods[2], (0.15^2) * (2 * 0.15 * 0.3))
})

test_that("kinLike IBS = 1 pattern 4", {
  qgt <- c(13, 13)
  rgt <- c(11, 13)
  afOneL <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  afAlOneL <- 11:15
  probIBD <- c(0.25, 0.5, 0.25)
  likelihoods <- kinLike(qgt, rgt, afOneL, afAlOneL, probIBD)
  expect_equal(likelihoods[1], 2 * 0.25 * (0.3^2) * 0.15 + 0.25 * (0.3^2) * (2 * 0.15 * 0.3))
  expect_equal(likelihoods[2], (0.3^2) * (2 * 0.15 * 0.3))
})

test_that("kinLike IBS = 2 pattern 1", {
  qgt <- c(11, 11)
  rgt <- c(11, 11)
  afOneL <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  afAlOneL <- 11:15
  probIBD <- c(0.25, 0.5, 0.25)
  likelihoods <- kinLike(qgt, rgt, afOneL, afAlOneL, probIBD)
  expect_equal(likelihoods[1], 0.25 * (0.15^2) + 2 * 0.25 * (0.15^2) * (0.15) + 0.25 * (0.15^2) * (0.15^2))
  expect_equal(likelihoods[2], (0.15^2) * (0.15^2))
})

test_that("kinLike IBS = 2 pattern 2", {
  qgt <- c(11, 12)
  rgt <- c(11, 12)
  afOneL <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  afAlOneL <- 11:15
  probIBD <- c(0.25, 0.5, 0.25)
  likelihoods <- kinLike(qgt, rgt, afOneL, afAlOneL, probIBD)
  expect_equal(likelihoods[1], 0.25 * (2 * 0.15 * 0.25) + 0.25 * (2 * 0.15 * 0.25) * (0.15 + 0.25) + 0.25 * (2 * 0.15 * 0.25) * (2 * 0.15 * 0.25))
  expect_equal(likelihoods[2], (2 * 0.15 * 0.25) * (2 * 0.15 * 0.25))
})

test_that("kinLike mutation pattern 1", {
  qgt <- c(11, 12)
  rgt <- c(13, 14)
  afOneL <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  afAlOneL <- 11:15
  probIBD <- c(0, 1, 0)
  mutation <- TRUE
  myuOneL <- 0.002
  apeOneL <- calcApe(afOneL)
  likelihoods <- kinLike(qgt, rgt, afOneL, afAlOneL, probIBD, mutation, myuOneL, apeOneL)
  expect_equal(likelihoods[1], 0.002)
  expect_equal(likelihoods[2], 0.352925)
})

test_that("kinLike mutation pattern 2", {
  qgt <- c(11, 12)
  rgt <- c(12, 14)
  afOneL <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  afAlOneL <- 11:15
  probIBD <- c(0, 1, 0)
  mutation <- TRUE
  myuOneL <- 0.002
  apeOneL <- calcApe(afOneL)
  likelihoods <- kinLike(qgt, rgt, afOneL, afAlOneL, probIBD, mutation, myuOneL, apeOneL)
  expect_equal(likelihoods[1], 0.5 * (2 * 0.15 * 0.25) * 0.25)
  expect_equal(likelihoods[2], (2 * 0.15 * 0.25) * (2 * 0.25 * 0.25))
})
