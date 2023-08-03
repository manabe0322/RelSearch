test_that("calc_kin_like_drop pattern 1", {

  # Condition
  vgt <- 11
  rgt <- c(11, 11)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  af_al <- 11:15
  k2 <- 0.25
  k1 <- 0.5 / 2
  k0 <- 0.25
  prob_ibd <- c(k2, 2 * k1, k0)
  cons_mu <- FALSE
  myu <- 0.002
  ape <- calc_ape(af)
  pd <- 0.3

  # True answer
  prob_vgt_h1_homo <- k2 + 2 * k1 * 0.15 + k0 * 0.15^2
  prob_vgt_h2_homo <- 0.15^2
  pd_vgt_homo <- (1 - pd)^2
  prob_vgt_hetero_h1 <- 2 * k1 * 0.85 + k0 * 2 * 0.15 * 0.85
  prob_vgt_hetero_h2 <- 2 * 0.15 * 0.85
  pd_vgt_hetero <- (1 - pd) * pd
  prob_rgt <- 0.15^2
  pd_rgt <- (1 - pd)^2
  like_h1_homo <- prob_vgt_h1_homo * pd_vgt_homo * prob_rgt * pd_rgt
  like_h1_hetero <- prob_vgt_hetero_h1 * pd_vgt_hetero * prob_rgt * pd_rgt
  like_h2_homo <- prob_vgt_h2_homo * pd_vgt_homo * prob_rgt * pd_rgt
  like_h2_hetero <- prob_vgt_hetero_h2 * pd_vgt_hetero * prob_rgt * pd_rgt
  like_h1 <- like_h1_homo + like_h1_hetero
  like_h2 <- like_h2_homo + like_h2_hetero

  # Run
  likelihoods <- calc_kin_like_drop(vgt, rgt, af, af_al, prob_ibd, cons_mu, myu, ape, pd)

  # Test
  expect_equal(as.numeric(likelihoods[1]), like_h1)
  expect_equal(as.numeric(likelihoods[2]), like_h2)
})

test_that("calc_kin_like_drop 2", {

  # Condition
  vgt <- 12
  rgt <- c(11, 11)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  af_al <- 11:15
  k2 <- 0.25
  k1 <- 0.5 / 2
  k0 <- 0.25
  prob_ibd <- c(k2, 2 * k1, k0)
  cons_mu <- FALSE
  myu <- 0.002
  ape <- calc_ape(af)
  pd <- 0.3

  # Run
  likelihoods <- calc_kin_like_drop(vgt, rgt, af, af_al, prob_ibd, cons_mu, myu, ape, pd)

  # True answer
  prob_vgt_h1_homo <- k0 * 0.25^2
  prob_vgt_h2_homo <- 0.25^2
  pd_vgt_homo <- (1 - pd)^2
  prob_vgt_hetero_h1 <- 2 * k1 * 0.25 + k0 * 2 * 0.15 * 0.25 + k0 * 2 * 0.25 * 0.6
  prob_vgt_hetero_h2 <- 2 * 0.15 * 0.25 + 2 * 0.25 * 0.6
  pd_vgt_hetero <- (1 - pd) * pd
  prob_rgt <- 0.15^2
  pd_rgt <- (1 - pd)^2
  like_h1_homo <- prob_vgt_h1_homo * pd_vgt_homo * prob_rgt * pd_rgt
  like_h1_hetero <- prob_vgt_hetero_h1 * pd_vgt_hetero * prob_rgt * pd_rgt
  like_h2_homo <- prob_vgt_h2_homo * pd_vgt_homo * prob_rgt * pd_rgt
  like_h2_hetero <- prob_vgt_hetero_h2 * pd_vgt_hetero * prob_rgt * pd_rgt
  like_h1 <- like_h1_homo + like_h1_hetero
  like_h2 <- like_h2_homo + like_h2_hetero

  # Test
  expect_equal(as.numeric(likelihoods[1]), like_h1)
  expect_equal(as.numeric(likelihoods[2]), like_h2)
})

test_that("calc_kin_like_drop 3", {
  vgt <- 11
  rgt <- c(11, 13)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  af_al <- 11:15
  k2 <- 0.25
  k1 <- 0.5 / 2
  k0 <- 0.25
  prob_ibd <- c(k2, 2 * k1, k0)
  cons_mu <- FALSE
  myu <- 0.002
  ape <- calc_ape(af)
  pd <- 0.5
  likelihoods <- calc_kin_like_drop(vgt, rgt, af, af_al, prob_ibd, cons_mu, myu, ape, pd)

  prob_rgt <- 2 * 0.15 * 0.3
  calc_homo_h1 <- (1 - pd) * prob_rgt * (k1 * 0.15 + k0 * 0.15^2)
  calc_hetero_h1 <- pd * prob_rgt * (k2 + k1 * 0.15 + k1 * 0.3 + k0 * 2 * 0.15 * 0.3 + k1 * 0.55 + k0 * 2 * 0.15 * 0.55)
  calc_homo_h2 <- (1 - pd) * prob_rgt * 0.15^2
  calc_hetero_h2 <- pd * prob_rgt * (2 * 0.15 * 0.3 + 2 * 0.15 * 0.55)

  expect_equal(as.numeric(likelihoods[1]), calc_homo_h1 + calc_hetero_h1)
  expect_equal(as.numeric(likelihoods[2]), calc_homo_h2 + calc_hetero_h2)
})

test_that("calc_kin_like_drop 4", {
  vgt <- 13
  rgt <- c(11, 13)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  af_al <- 11:15
  k2 <- 0.25
  k1 <- 0.5 / 2
  k0 <- 0.25
  prob_ibd <- c(k2, 2 * k1, k0)
  cons_mu <- FALSE
  myu <- 0.002
  ape <- calc_ape(af)
  pd <- 0.5
  likelihoods <- calc_kin_like_drop(vgt, rgt, af, af_al, prob_ibd, cons_mu, myu, ape, pd)

  prob_rgt <- 2 * 0.15 * 0.3
  calc_homo_h1 <- (1 - pd) * prob_rgt * (k1 * 0.3 + k0 * 0.3^2)
  calc_hetero_h1 <- pd * prob_rgt * (k2 + k1 * 0.15 + k1 * 0.3 + k0 * 2 * 0.15 * 0.3 + k1 * 0.55 + k0 * 2 * 0.3 * 0.55)
  calc_homo_h2 <- (1 - pd) * prob_rgt * 0.3^2
  calc_hetero_h2 <- pd * prob_rgt * (2 * 0.15 * 0.3 + 2 * 0.3 * 0.55)

  expect_equal(as.numeric(likelihoods[1]), calc_homo_h1 + calc_hetero_h1)
  expect_equal(as.numeric(likelihoods[2]), calc_homo_h2 + calc_hetero_h2)
})

test_that("calc_kin_like_drop 5", {
  vgt <- 11
  rgt <- c(12, 13)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  af_al <- 11:15
  k2 <- 0.25
  k1 <- 0.5 / 2
  k0 <- 0.25
  prob_ibd <- c(k2, 2 * k1, k0)
  cons_mu <- FALSE
  myu <- 0.002
  ape <- calc_ape(af)
  pd <- 0.5
  likelihoods <- calc_kin_like_drop(vgt, rgt, af, af_al, prob_ibd, cons_mu, myu, ape, pd)

  prob_rgt <- 2 * 0.25 * 0.3
  calc_homo_h1 <- (1 - pd) * prob_rgt * (k0 * 0.15^2)
  calc_hetero_h1 <- pd * prob_rgt * (k1 * 0.15 + k0 * 2 * 0.15 * 0.25 + k1 * 0.15 + k0 * 2 * 0.15 * 0.3 + k0 * 2 * 0.15 * 0.3)
  calc_homo_h2 <- (1 - pd) * prob_rgt * 0.15^2
  calc_hetero_h2 <- pd * prob_rgt * (2 * 0.15 * 0.25 + 2 * 0.15 * 0.3 + 2 * 0.15 * 0.3)

  expect_equal(as.numeric(likelihoods[1]), calc_homo_h1 + calc_hetero_h1)
  expect_equal(as.numeric(likelihoods[2]), calc_homo_h2 + calc_hetero_h2)
})

test_that("calc_kin_like_drop mutation pattern 1 (actually not used)", {
  vgt <- 11
  rgt <- c(12, 12)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  af_al <- 11:15
  prob_ibd <- c(0, 1, 0)
  cons_mu <- TRUE
  myu <- 0.002
  ape <- calc_ape(af)
  pd <- 0.5
  likelihoods <- calc_kin_like_drop(vgt, rgt, af, af_al, prob_ibd, cons_mu, myu, ape, pd)

  prob_rgt <- 0.25^2
  calc_homo_h1 <- (1 - pd) * myu
  calc_hetero_h1 <- pd * (prob_rgt * 0.15 + myu)
  calc_homo_h2 <- (1 - pd) * ape
  calc_hetero_h2 <- pd * (prob_rgt * 2 * 0.15 * 0.25 + ape)

  expect_equal(as.numeric(likelihoods[1]), calc_homo_h1 + calc_hetero_h1)
  expect_equal(as.numeric(likelihoods[2]), calc_homo_h2 + calc_hetero_h2)
})

test_that("calc_kin_like_drop mutation pattern 2", {
  vgt <- 12
  rgt <- c(11, 13)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  af_al <- 11:15
  prob_ibd <- c(0, 1, 0)
  cons_mu <- FALSE
  myu <- 0.002
  ape <- calc_ape(af)
  pd <- 0.5
  likelihoods <- calc_kin_like_drop(vgt, rgt, af, af_al, prob_ibd, cons_mu, myu, ape, pd)

  prob_rgt <- 2 * 0.15 * 0.3
  calc_homo_h1 <- 0
  calc_hetero_h1 <- pd * prob_rgt * (0.5 * 0.25 + 0.5 * 0.25)
  calc_homo_h2 <- (1 - pd) * prob_rgt * 0.25^2
  calc_hetero_h2 <- pd * prob_rgt * (2 * 0.15 * 0.25 + 2 * 0.25 * 0.3 + 2 * 0.25 * 0.3)

  expect_equal(as.numeric(likelihoods[1]), calc_homo_h1 + calc_hetero_h1)
  expect_equal(as.numeric(likelihoods[2]), calc_homo_h2 + calc_hetero_h2)
})

