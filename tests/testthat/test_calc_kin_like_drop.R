test_that("calc_kin_like_drop vgt pattern 1", {

  # Condition
  vgt <- 11
  rgt <- c(11, 11)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  af_al <- 11:15
  k2 <- 0.25
  k1 <- 0.5 / 2
  k0 <- 0.25
  pibd <- c(k2, 2 * k1, k0)
  cons_mu <- FALSE
  myu <- 0.002
  ape <- calc_ape(af)
  pd_v <- 0.3
  pd_r <- 0.1

  # True answer

  ## vgt : 11, 11
  pvgt_h1_1 <- k2 + 2 * k1 * 0.15 + k0 * 0.15^2
  pvgt_h2_1 <- 0.15^2

  ## vgt : 11, 99
  pvgt_h1_2 <- 2 * k1 * 0.85 + k0 * 2 * 0.15 * 0.85
  pvgt_h2_2 <- 2 * 0.15 * 0.85

  ## vgt dropout
  pd_vgt_homo <- (1 - pd_v)^2
  pd_vgt_hetero <- (1 - pd_v) * pd_v

  ## rgt : 11, 11
  prgt <- 0.15^2

  ## rgt dropout
  pd_rgt <- (1 - pd_r)^2

  ## Likelihood
  like_h1 <- pvgt_h1_1 * prgt * pd_vgt_homo * pd_rgt + pvgt_h1_2 * prgt * pd_vgt_hetero * pd_rgt
  like_h2 <- pvgt_h2_1 * prgt * pd_vgt_homo * pd_rgt + pvgt_h2_2 * prgt * pd_vgt_hetero * pd_rgt

  # Run
  likelihoods <- calc_kin_like_drop(vgt, rgt, af, af_al, pibd, cons_mu, myu, ape, pd_v, pd_r)

  # Test
  expect_equal(as.numeric(likelihoods[1]), like_h1)
  expect_equal(as.numeric(likelihoods[2]), like_h2)
})

test_that("calc_kin_like_drop vgt pattern 2", {

  # Condition
  vgt <- 12
  rgt <- c(11, 11)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  af_al <- 11:15
  k2 <- 0.25
  k1 <- 0.5 / 2
  k0 <- 0.25
  pibd <- c(k2, 2 * k1, k0)
  cons_mu <- FALSE
  myu <- 0.002
  ape <- calc_ape(af)
  pd_v <- 0.3
  pd_r <- 0.1

  # True answer

  ## vgt : 12, 12
  pvgt_h1_1 <- k0 * 0.25^2
  pvgt_h2_1 <- 0.25^2

  ## vgt : 11, 12
  pvgt_h1_2 <- 2 * k1 * 0.25 + k0 * 2 * 0.15 * 0.25
  pvgt_h2_2 <- 2 * 0.15 * 0.25

  ## vgt : 12, 99
  pvgt_h1_3 <- k0 * 2 * 0.25 * 0.6
  pvgt_h2_3 <- 2 * 0.25 * 0.6

  ## vgt dropout
  pd_vgt_homo <- (1 - pd_v)^2
  pd_vgt_hetero <- (1 - pd_v) * pd_v

  ## rgt : 11, 11
  prgt <- 0.15^2

  ## rgt dropout
  pd_rgt <- (1 - pd_r)^2

  ## Likelihood
  like_h1 <- pvgt_h1_1 * prgt * pd_vgt_homo * pd_rgt + (pvgt_h1_2 + pvgt_h1_3) * prgt * pd_vgt_hetero * pd_rgt
  like_h2 <- pvgt_h2_1 * prgt * pd_vgt_homo * pd_rgt + (pvgt_h2_2 + pvgt_h2_3) * prgt * pd_vgt_hetero * pd_rgt

  # Run
  likelihoods <- calc_kin_like_drop(vgt, rgt, af, af_al, pibd, cons_mu, myu, ape, pd_v, pd_r)

  # Test
  expect_equal(as.numeric(likelihoods[1]), like_h1)
  expect_equal(as.numeric(likelihoods[2]), like_h2)
})

test_that("calc_kin_like_drop vgt pattern 3", {

  # Condition
  vgt <- 11
  rgt <- c(11, 13)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  af_al <- 11:15
  k2 <- 0.25
  k1 <- 0.5 / 2
  k0 <- 0.25
  pibd <- c(k2, 2 * k1, k0)
  cons_mu <- FALSE
  myu <- 0.002
  ape <- calc_ape(af)
  pd_v <- 0.3
  pd_r <- 0.1

  # True answer

  ## vgt : 11, 11
  pvgt_h1_1 <- k1 * 0.15 + k0 * 0.15^2
  pvgt_h2_1 <- 0.15^2

  ## vgt : 11, 13
  pvgt_h1_2 <- k2 + k1 * 0.3 + k1 * 0.15 + k0 * 2 * 0.15 * 0.3
  pvgt_h2_2 <- 2 * 0.15 * 0.3

  ## vgt : 11, 99
  pvgt_h1_3 <- k1 * 0.55 + k0 * 2 * 0.15 * 0.55
  pvgt_h2_3 <- 2 * 0.15 * 0.55

  ## vgt dropout
  pd_vgt_homo <- (1 - pd_v)^2
  pd_vgt_hetero <- (1 - pd_v) * pd_v

  ## rgt : 11, 13
  prgt <- 2 * 0.15 * 0.3

  ## rgt dropout
  pd_rgt <- (1 - pd_r)^2

  ## Likelihood
  like_h1 <- pvgt_h1_1 * prgt * pd_vgt_homo * pd_rgt + (pvgt_h1_2 + pvgt_h1_3) * prgt * pd_vgt_hetero * pd_rgt
  like_h2 <- pvgt_h2_1 * prgt * pd_vgt_homo * pd_rgt + (pvgt_h2_2 + pvgt_h2_3) * prgt * pd_vgt_hetero * pd_rgt

  # Run
  likelihoods <- calc_kin_like_drop(vgt, rgt, af, af_al, pibd, cons_mu, myu, ape, pd_v, pd_r)

  # Test
  expect_equal(as.numeric(likelihoods[1]), like_h1)
  expect_equal(as.numeric(likelihoods[2]), like_h2)
})

test_that("calc_kin_like_drop vgt pattern 4", {

  # Condition
  vgt <- 13
  rgt <- c(11, 13)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  af_al <- 11:15
  k2 <- 0.25
  k1 <- 0.5 / 2
  k0 <- 0.25
  pibd <- c(k2, 2 * k1, k0)
  cons_mu <- FALSE
  myu <- 0.002
  ape <- calc_ape(af)
  pd_v <- 0.3
  pd_r <- 0.1

  # True answer

  ## vgt : 13, 13
  pvgt_h1_1 <- k1 * 0.3 + k0 * 0.3^2
  pvgt_h2_1 <- 0.3^2

  ## vgt : 11, 13
  pvgt_h1_2 <- k2 + k1 * 0.3 + k1 * 0.15 + k0 * 2 * 0.15 * 0.3
  pvgt_h2_2 <- 2 * 0.15 * 0.3

  ## vgt : 13, 99
  pvgt_h1_3 <- k1 * 0.55 + k0 * 2 * 0.3 * 0.55
  pvgt_h2_3 <- 2 * 0.3 * 0.55

  ## vgt dropout
  pd_vgt_homo <- (1 - pd_v)^2
  pd_vgt_hetero <- (1 - pd_v) * pd_v

  ## rgt : 11, 13
  prgt <- 2 * 0.15 * 0.3

  ## rgt dropout
  pd_rgt <- (1 - pd_r)^2

  ## Likelihood
  like_h1 <- pvgt_h1_1 * prgt * pd_vgt_homo * pd_rgt + (pvgt_h1_2 + pvgt_h1_3) * prgt * pd_vgt_hetero * pd_rgt
  like_h2 <- pvgt_h2_1 * prgt * pd_vgt_homo * pd_rgt + (pvgt_h2_2 + pvgt_h2_3) * prgt * pd_vgt_hetero * pd_rgt

  # Run
  likelihoods <- calc_kin_like_drop(vgt, rgt, af, af_al, pibd, cons_mu, myu, ape, pd_v, pd_r)

  # Test
  expect_equal(as.numeric(likelihoods[1]), like_h1)
  expect_equal(as.numeric(likelihoods[2]), like_h2)
})

test_that("calc_kin_like_drop vgt pattern 5", {

  # Condition
  vgt <- 11
  rgt <- c(12, 13)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  af_al <- 11:15
  k2 <- 0.25
  k1 <- 0.5 / 2
  k0 <- 0.25
  pibd <- c(k2, 2 * k1, k0)
  cons_mu <- FALSE
  myu <- 0.002
  ape <- calc_ape(af)
  pd_v <- 0.3
  pd_r <- 0.1

  # True answer

  ## vgt : 11, 11
  pvgt_h1_1 <- k0 * 0.15^2
  pvgt_h2_1 <- 0.15^2

  ## vgt : 11, 12
  pvgt_h1_2 <- k1 * 0.15 + k0 * 2 * 0.15 * 0.25
  pvgt_h2_2 <- 2 * 0.15 * 0.25

  ## vgt : 11, 13
  pvgt_h1_3 <- k1 * 0.15 + k0 * 2 * 0.15 * 0.3
  pvgt_h2_3 <- 2 * 0.15 * 0.3

  ## vgt : 11, 99
  pvgt_h1_4 <- k0 * 2 * 0.15 * 0.3
  pvgt_h2_4 <- 2 * 0.15 * 0.3

  ## vgt dropout
  pd_vgt_homo <- (1 - pd_v)^2
  pd_vgt_hetero <- (1 - pd_v) * pd_v

  ## rgt : 12, 13
  prgt <- 2 * 0.25 * 0.3

  ## rgt dropout
  pd_rgt <- (1 - pd_r)^2

  ## Likelihood
  like_h1 <- pvgt_h1_1 * prgt * pd_vgt_homo * pd_rgt + (pvgt_h1_2 + pvgt_h1_3 + pvgt_h1_4) * prgt * pd_vgt_hetero * pd_rgt
  like_h2 <- pvgt_h2_1 * prgt * pd_vgt_homo * pd_rgt + (pvgt_h2_2 + pvgt_h2_3 + pvgt_h2_4) * prgt * pd_vgt_hetero * pd_rgt

  # Run
  likelihoods <- calc_kin_like_drop(vgt, rgt, af, af_al, pibd, cons_mu, myu, ape, pd_v, pd_r)

  # Test
  expect_equal(as.numeric(likelihoods[1]), like_h1)
  expect_equal(as.numeric(likelihoods[2]), like_h2)
})

test_that("calc_kin_like_drop rgt pattern 1", {

  # Condition
  vgt <- c(11, 11)
  rgt <- 11
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  af_al <- 11:15
  k2 <- 0.25
  k1 <- 0.5 / 2
  k0 <- 0.25
  pibd <- c(k2, 2 * k1, k0)
  cons_mu <- FALSE
  myu <- 0.002
  ape <- calc_ape(af)
  pd_v <- 0.3
  pd_r <- 0.1

  # True answer

  ## vgt : 11, 11, rgt : 11, 11
  pvgt_h1_1 <- k2 + 2 * k1 * 0.15 + k0 * 0.15^2
  pvgt_h2_1 <- 0.15^2
  prgt_1 <- 0.15^2

  ## vgt : 11, 11, rgt : 11, 99
  pvgt_h1_2 <- k1 * 0.15 + k0 * 0.15^2
  pvgt_h2_2 <- 0.15^2
  prgt_2 <- 2 * 0.15 * 0.85

  ## vgt dropout
  pd_vgt <- (1 - pd_v)^2

  ## rgt dropout
  pd_rgt_homo <- (1 - pd_r)^2
  pd_rgt_hetero <- (1 - pd_r) * pd_r

  ## Likelihood
  like_h1 <- pvgt_h1_1 * prgt_1 * pd_vgt * pd_rgt_homo + pvgt_h1_2 * prgt_2 * pd_vgt * pd_rgt_hetero
  like_h2 <- pvgt_h2_1 * prgt_1 * pd_vgt * pd_rgt_homo + pvgt_h2_2 * prgt_2 * pd_vgt * pd_rgt_hetero

  # Run
  likelihoods <- calc_kin_like_drop(vgt, rgt, af, af_al, pibd, cons_mu, myu, ape, pd_v, pd_r)

  # Test
  expect_equal(as.numeric(likelihoods[1]), like_h1)
  expect_equal(as.numeric(likelihoods[2]), like_h2)
})

test_that("calc_kin_like_drop rgt pattern 2", {

  # Condition
  vgt <- c(11, 11)
  rgt <- 12
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  af_al <- 11:15
  k2 <- 0.25
  k1 <- 0.5 / 2
  k0 <- 0.25
  pibd <- c(k2, 2 * k1, k0)
  cons_mu <- FALSE
  myu <- 0.002
  ape <- calc_ape(af)
  pd_v <- 0.3
  pd_r <- 0.1

  # True answer

  ## vgt : 11, 11, rgt : 12, 12
  pvgt_h1_1 <- k0 * 0.15^2
  pvgt_h2_1 <- 0.15^2
  prgt_1 <- 0.25^2

  ## vgt : 11, 11, rgt : 11, 12
  pvgt_h1_2 <- k1 * 0.15 + k0 * 0.15^2
  pvgt_h2_2 <- 0.15^2
  prgt_2 <- 2 * 0.15 * 0.25

  ## vgt : 11, 11, rgt : 12, 99
  pvgt_h1_3 <- k0 * 0.15^2
  pvgt_h2_3 <- 0.15^2
  prgt_3 <- 2 * 0.25 * 0.6

  ## vgt dropout
  pd_vgt <- (1 - pd_v)^2

  ## rgt dropout
  pd_rgt_homo <- (1 - pd_r)^2
  pd_rgt_hetero <- (1 - pd_r) * pd_r

  ## Likelihood
  like_h1 <- pvgt_h1_1 * prgt_1 * pd_vgt * pd_rgt_homo + pvgt_h1_2 * prgt_2 * pd_vgt * pd_rgt_hetero + pvgt_h1_3 * prgt_3 * pd_vgt * pd_rgt_hetero
  like_h2 <- pvgt_h2_1 * prgt_1 * pd_vgt * pd_rgt_homo + pvgt_h2_2 * prgt_2 * pd_vgt * pd_rgt_hetero + pvgt_h2_3 * prgt_3 * pd_vgt * pd_rgt_hetero

  # Run
  likelihoods <- calc_kin_like_drop(vgt, rgt, af, af_al, pibd, cons_mu, myu, ape, pd_v, pd_r)

  # Test
  expect_equal(as.numeric(likelihoods[1]), like_h1)
  expect_equal(as.numeric(likelihoods[2]), like_h2)
})

test_that("calc_kin_like_drop rgt pattern 3", {

  # Condition
  vgt <- c(11, 13)
  rgt <- 11
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  af_al <- 11:15
  k2 <- 0.25
  k1 <- 0.5 / 2
  k0 <- 0.25
  pibd <- c(k2, 2 * k1, k0)
  cons_mu <- FALSE
  myu <- 0.002
  ape <- calc_ape(af)
  pd_v <- 0.3
  pd_r <- 0.1

  # True answer

  ## vgt : 11, 13, rgt : 11, 11
  pvgt_h1_1 <- 2 * k1 * 0.3 + k0 * 2 * 0.15 * 0.3
  pvgt_h2_1 <- 2 * 0.15 * 0.3
  prgt_1 <- 0.15^2

  ## vgt : 11, 13, rgt : 11, 13
  pvgt_h1_2 <- k2 + k1 * 0.15 + k1 * 0.3 + k0 * 2 * 0.15 * 0.3
  pvgt_h2_2 <- 2 * 0.15 * 0.3
  prgt_2 <- 2 * 0.15 * 0.3

  ## vgt : 11, 13, rgt : 11, 99
  pvgt_h1_3 <- k1 * 0.3 + k0 * 2 * 0.15 * 0.3
  pvgt_h2_3 <- 2 * 0.15 * 0.3
  prgt_3 <- 2 * 0.15 * 0.55

  ## vgt dropout
  pd_vgt <- (1 - pd_v)^2

  ## rgt dropout
  pd_rgt_homo <- (1 - pd_r)^2
  pd_rgt_hetero <- (1 - pd_r) * pd_r

  ## Likelihood
  like_h1 <- pvgt_h1_1 * prgt_1 * pd_vgt * pd_rgt_homo + pvgt_h1_2 * prgt_2 * pd_vgt * pd_rgt_hetero + pvgt_h1_3 * prgt_3 * pd_vgt * pd_rgt_hetero
  like_h2 <- pvgt_h2_1 * prgt_1 * pd_vgt * pd_rgt_homo + pvgt_h2_2 * prgt_2 * pd_vgt * pd_rgt_hetero + pvgt_h2_3 * prgt_3 * pd_vgt * pd_rgt_hetero

  # Run
  likelihoods <- calc_kin_like_drop(vgt, rgt, af, af_al, pibd, cons_mu, myu, ape, pd_v, pd_r)

  # Test
  expect_equal(as.numeric(likelihoods[1]), like_h1)
  expect_equal(as.numeric(likelihoods[2]), like_h2)
})

test_that("calc_kin_like_drop rgt pattern 4", {

  # Condition
  vgt <- c(11, 13)
  rgt <- 13
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  af_al <- 11:15
  k2 <- 0.25
  k1 <- 0.5 / 2
  k0 <- 0.25
  pibd <- c(k2, 2 * k1, k0)
  cons_mu <- FALSE
  myu <- 0.002
  ape <- calc_ape(af)
  pd_v <- 0.3
  pd_r <- 0.1

  # True answer

  ## vgt : 11, 13, rgt : 13, 13
  pvgt_h1_1 <- 2 * k1 * 0.15 + k0 * 2 * 0.15 * 0.3
  pvgt_h2_1 <- 2 * 0.15 * 0.3
  prgt_1 <- 0.3^2

  ## vgt : 11, 13, rgt : 11, 13
  pvgt_h1_2 <- k2 + k1 * 0.15 + k1 * 0.3 + k0 * 2 * 0.15 * 0.3
  pvgt_h2_2 <- 2 * 0.15 * 0.3
  prgt_2 <- 2 * 0.15 * 0.3

  ## vgt : 11, 13, rgt : 13, 99
  pvgt_h1_3 <- k1 * 0.15 + k0 * 2 * 0.15 * 0.3
  pvgt_h2_3 <- 2 * 0.15 * 0.3
  prgt_3 <- 2 * 0.3 * 0.55

  ## vgt dropout
  pd_vgt <- (1 - pd_v)^2

  ## rgt dropout
  pd_rgt_homo <- (1 - pd_r)^2
  pd_rgt_hetero <- (1 - pd_r) * pd_r

  ## Likelihood
  like_h1 <- pvgt_h1_1 * prgt_1 * pd_vgt * pd_rgt_homo + pvgt_h1_2 * prgt_2 * pd_vgt * pd_rgt_hetero + pvgt_h1_3 * prgt_3 * pd_vgt * pd_rgt_hetero
  like_h2 <- pvgt_h2_1 * prgt_1 * pd_vgt * pd_rgt_homo + pvgt_h2_2 * prgt_2 * pd_vgt * pd_rgt_hetero + pvgt_h2_3 * prgt_3 * pd_vgt * pd_rgt_hetero

  # Run
  likelihoods <- calc_kin_like_drop(vgt, rgt, af, af_al, pibd, cons_mu, myu, ape, pd_v, pd_r)

  # Test
  expect_equal(as.numeric(likelihoods[1]), like_h1)
  expect_equal(as.numeric(likelihoods[2]), like_h2)
})

test_that("calc_kin_like_drop rgt pattern 5", {

  # Condition
  vgt <- c(12, 13)
  rgt <- 11
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  af_al <- 11:15
  k2 <- 0.25
  k1 <- 0.5 / 2
  k0 <- 0.25
  pibd <- c(k2, 2 * k1, k0)
  cons_mu <- FALSE
  myu <- 0.002
  ape <- calc_ape(af)
  pd_v <- 0.3
  pd_r <- 0.1

  # True answer

  ## vgt : 12, 13, rgt : 11, 11
  pvgt_h1_1 <- k0 * 2 * 0.25 * 0.3
  pvgt_h2_1 <- 2 * 0.25 * 0.3
  prgt_1 <- 0.15^2

  ## vgt : 12, 13, rgt : 11, 12
  pvgt_h1_2 <- k1 * 0.3 + k0 * 2 * 0.25 * 0.3
  pvgt_h2_2 <- 2 * 0.25 * 0.3
  prgt_2 <- 2 * 0.15 * 0.25

  ## vgt : 12, 13, rgt : 11, 13
  pvgt_h1_3 <- k1 * 0.25 + k0 * 2 * 0.25 * 0.3
  pvgt_h2_3 <- 2 * 0.25 * 0.3
  prgt_3 <- 2 * 0.15 * 0.3

  ## vgt : 12, 13, rgt : 11, 99
  pvgt_h1_4 <- k0 * 2 * 0.25 * 0.3
  pvgt_h2_4 <- 2 * 0.25 * 0.3
  prgt_4 <- 2 * 0.15 * 0.3

  ## vgt dropout
  pd_vgt <- (1 - pd_v)^2

  ## rgt dropout
  pd_rgt_homo <- (1 - pd_r)^2
  pd_rgt_hetero <- (1 - pd_r) * pd_r

  ## Likelihood
  like_h1 <- pvgt_h1_1 * prgt_1 * pd_vgt * pd_rgt_homo + pvgt_h1_2 * prgt_2 * pd_vgt * pd_rgt_hetero + pvgt_h1_3 * prgt_3 * pd_vgt * pd_rgt_hetero + pvgt_h1_4 * prgt_4 * pd_vgt * pd_rgt_hetero
  like_h2 <- pvgt_h2_1 * prgt_1 * pd_vgt * pd_rgt_homo + pvgt_h2_2 * prgt_2 * pd_vgt * pd_rgt_hetero + pvgt_h2_3 * prgt_3 * pd_vgt * pd_rgt_hetero + pvgt_h2_4 * prgt_4 * pd_vgt * pd_rgt_hetero

  # Run
  likelihoods <- calc_kin_like_drop(vgt, rgt, af, af_al, pibd, cons_mu, myu, ape, pd_v, pd_r)

  # Test
  expect_equal(as.numeric(likelihoods[1]), like_h1)
  expect_equal(as.numeric(likelihoods[2]), like_h2)
})

test_that("calc_kin_like_drop designated two alleles pattern 1", {

  # Condition
  vgt <- c(11, 11)
  rgt <- c(11, 11)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  af_al <- 11:15
  k2 <- 0.25
  k1 <- 0.5 / 2
  k0 <- 0.25
  pibd <- c(k2, 2 * k1, k0)
  cons_mu <- FALSE
  myu <- 0.002
  ape <- calc_ape(af)
  pd_v <- 0.3
  pd_r <- 0.1

  # True answer

  ## vgt : 11, 11
  pvgt_h1_1 <- k2 + 2 * k1 * 0.15 + k0 * 0.15^2
  pvgt_h2_1 <- 0.15^2

  ## vgt dropout
  pd_vgt <- (1 - pd_v)^2

  ## rgt : 11, 11
  prgt <- 0.15^2

  ## rgt dropout
  pd_rgt <- (1 - pd_r)^2

  ## Likelihood
  like_h1 <- pvgt_h1_1 * prgt * pd_vgt * pd_rgt
  like_h2 <- pvgt_h2_1 * prgt * pd_vgt * pd_rgt

  # Run
  likelihoods <- calc_kin_like_drop(vgt, rgt, af, af_al, pibd, cons_mu, myu, ape, pd_v, pd_r)

  # Test
  expect_equal(as.numeric(likelihoods[1]), like_h1)
  expect_equal(as.numeric(likelihoods[2]), like_h2)
})

test_that("calc_kin_like_drop designated two alleles pattern 2", {

  # Condition
  vgt <- c(11, 11)
  rgt <- c(11, 12)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  af_al <- 11:15
  k2 <- 0.25
  k1 <- 0.5 / 2
  k0 <- 0.25
  pibd <- c(k2, 2 * k1, k0)
  cons_mu <- FALSE
  myu <- 0.002
  ape <- calc_ape(af)
  pd_v <- 0.3
  pd_r <- 0.1

  # True answer

  ## vgt : 11, 11
  pvgt_h1_1 <- k1 * 0.15 + k0 * 0.15^2
  pvgt_h2_1 <- 0.15^2

  ## vgt dropout
  pd_vgt <- (1 - pd_v)^2

  ## rgt : 11, 12
  prgt <- 2 * 0.15 * 0.25

  ## rgt dropout
  pd_rgt <- (1 - pd_r)^2

  ## Likelihood
  like_h1 <- pvgt_h1_1 * prgt * pd_vgt * pd_rgt
  like_h2 <- pvgt_h2_1 * prgt * pd_vgt * pd_rgt

  # Run
  likelihoods <- calc_kin_like_drop(vgt, rgt, af, af_al, pibd, cons_mu, myu, ape, pd_v, pd_r)

  # Test
  expect_equal(as.numeric(likelihoods[1]), like_h1)
  expect_equal(as.numeric(likelihoods[2]), like_h2)
})

test_that("calc_kin_like_drop designated two alleles pattern 3", {

  # Condition
  vgt <- c(11, 12)
  rgt <- c(11, 12)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  af_al <- 11:15
  k2 <- 0.25
  k1 <- 0.5 / 2
  k0 <- 0.25
  pibd <- c(k2, 2 * k1, k0)
  cons_mu <- FALSE
  myu <- 0.002
  ape <- calc_ape(af)
  pd_v <- 0.3
  pd_r <- 0.1

  # True answer

  ## vgt : 11, 12
  pvgt_h1_1 <- k2 + k1 * 0.15 + k1 * 0.25 + k0 * 2 * 0.15 * 0.25
  pvgt_h2_1 <- 2 * 0.15 * 0.25

  ## vgt dropout
  pd_vgt <- (1 - pd_v)^2

  ## rgt : 11, 12
  prgt <- 2 * 0.15 * 0.25

  ## rgt dropout
  pd_rgt <- (1 - pd_r)^2

  ## Likelihood
  like_h1 <- pvgt_h1_1 * prgt * pd_vgt * pd_rgt
  like_h2 <- pvgt_h2_1 * prgt * pd_vgt * pd_rgt

  # Run
  likelihoods <- calc_kin_like_drop(vgt, rgt, af, af_al, pibd, cons_mu, myu, ape, pd_v, pd_r)

  # Test
  expect_equal(as.numeric(likelihoods[1]), like_h1)
  expect_equal(as.numeric(likelihoods[2]), like_h2)
})

test_that("calc_kin_like_drop Pr(D) = 0 pattern 1", {

  # Condition
  vgt <- c(11, 11)
  rgt <- c(12, 12)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  af_al <- 11:15
  k2 <- 0.25
  k1 <- 0.5 / 2
  k0 <- 0.25
  pibd <- c(k2, 2 * k1, k0)
  cons_mu <- FALSE
  myu <- 0.002
  ape <- calc_ape(af)
  pd_v <- 0
  pd_r <- 0

  # Run
  likelihoods_1 <- calc_kin_like_drop(vgt, rgt, af, af_al, pibd, cons_mu, myu, ape, pd_v, pd_r)
  likelihoods_2 <- calc_kin_like(vgt, rgt, af, af_al, pibd, cons_mu, myu, ape)

  # Test
  expect_equal(as.numeric(likelihoods_1[1]), as.numeric(likelihoods_1[1]))
  expect_equal(as.numeric(likelihoods_2[2]), as.numeric(likelihoods_2[2]))
})

test_that("calc_kin_like_drop Pr(D) = 0 pattern 2", {

  # Condition
  vgt <- c(11, 11)
  rgt <- c(11, 12)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  af_al <- 11:15
  k2 <- 0.25
  k1 <- 0.5 / 2
  k0 <- 0.25
  pibd <- c(k2, 2 * k1, k0)
  cons_mu <- FALSE
  myu <- 0.002
  ape <- calc_ape(af)
  pd_v <- 0
  pd_r <- 0

  # Run
  likelihoods_1 <- calc_kin_like_drop(vgt, rgt, af, af_al, pibd, cons_mu, myu, ape, pd_v, pd_r)
  likelihoods_2 <- calc_kin_like(vgt, rgt, af, af_al, pibd, cons_mu, myu, ape)

  # Test
  expect_equal(as.numeric(likelihoods_1[1]), as.numeric(likelihoods_1[1]))
  expect_equal(as.numeric(likelihoods_2[2]), as.numeric(likelihoods_2[2]))
})

test_that("calc_kin_like_drop Pr(D) = 0 pattern 3", {

  # Condition
  vgt <- c(11, 12)
  rgt <- c(11, 12)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  af_al <- 11:15
  k2 <- 0.25
  k1 <- 0.5 / 2
  k0 <- 0.25
  pibd <- c(k2, 2 * k1, k0)
  cons_mu <- FALSE
  myu <- 0.002
  ape <- calc_ape(af)
  pd_v <- 0
  pd_r <- 0

  # Run
  likelihoods_1 <- calc_kin_like_drop(vgt, rgt, af, af_al, pibd, cons_mu, myu, ape, pd_v, pd_r)
  likelihoods_2 <- calc_kin_like(vgt, rgt, af, af_al, pibd, cons_mu, myu, ape)

  # Test
  expect_equal(as.numeric(likelihoods_1[1]), as.numeric(likelihoods_1[1]))
  expect_equal(as.numeric(likelihoods_2[2]), as.numeric(likelihoods_2[2]))
})




test_that("calc_kin_like_drop mutation pattern 1", {

  # Condition
  vgt <- 11
  rgt <- c(12, 12)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  af_al <- 11:15
  k2 <- 0
  k1 <- 0.5
  k0 <- 0
  pibd <- c(k2, 2 * k1, k0)
  cons_mu <- TRUE
  myu <- 0.002
  ape <- calc_ape(af)
  pd_v <- 0.3
  pd_r <- 0.1

  # True answer

  ## vgt : 11, 11
  pvgt_h1_1 <- myu
  pvgt_h2_1 <- ape

  ## vgt : 11, 12
  pvgt_h1_2 <- 2 * k1 * 0.15 + k0 * 2 * 0.15 * 0.25
  pvgt_h2_2 <- 2 * 0.15 * 0.25

  ## vgt : 11, 99
  pvgt_h1_3 <- myu
  pvgt_h2_3 <- ape

  ## vgt dropout
  pd_vgt_homo <- (1 - pd_v)^2
  pd_vgt_hetero <- (1 - pd_v) * pd_v

  ## rgt : 12, 12
  prgt <- 0.25^2

  ## rgt dropout
  pd_rgt <- (1 - pd_r)^2

  ## Likelihood
  like_h1 <- pvgt_h1_1 * prgt * pd_vgt_homo * pd_rgt + (pvgt_h1_2 + pvgt_h1_3) * prgt * pd_vgt_hetero * pd_rgt
  like_h2 <- pvgt_h2_1 * prgt * pd_vgt_homo * pd_rgt + (pvgt_h2_2 + pvgt_h2_3) * prgt * pd_vgt_hetero * pd_rgt

  # Run
  likelihoods <- calc_kin_like_drop(vgt, rgt, af, af_al, pibd, cons_mu, myu, ape, pd_v, pd_r)

  likelihoods_2 <- calc_kin_like(vgt, rgt, af, af_al, pibd, cons_mu, myu, ape)

  # Test
  expect_equal(as.numeric(likelihoods[1]), like_h1)
  expect_equal(as.numeric(likelihoods[2]), like_h2)
})

test_that("calc_kin_like_drop mutation pattern 2", {
  vgt <- 12
  rgt <- c(11, 13)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  af_al <- 11:15
  pibd <- c(0, 1, 0)
  cons_mu <- FALSE
  myu <- 0.002
  ape <- calc_ape(af)
  pd <- 0.5
  likelihoods <- calc_kin_like_drop(vgt, rgt, af, af_al, pibd, cons_mu, myu, ape, pd)

  prgt <- 2 * 0.15 * 0.3
  calc_homo_h1 <- 0
  calc_hetero_h1 <- pd * prgt * (0.5 * 0.25 + 0.5 * 0.25)
  calc_homo_h2 <- (1 - pd) * prgt * 0.25^2
  calc_hetero_h2 <- pd * prgt * (2 * 0.15 * 0.25 + 2 * 0.25 * 0.3 + 2 * 0.25 * 0.3)

  expect_equal(as.numeric(likelihoods[1]), calc_homo_h1 + calc_hetero_h1)
  expect_equal(as.numeric(likelihoods[2]), calc_homo_h2 + calc_hetero_h2)
})

