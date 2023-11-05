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
  myu <- 0.002
  cons_mu <- FALSE
  par_vic <- FALSE

  # True answer

  ## vgt : 11, 11
  pvgt_h1_1 <- k2 + 2 * k1 * 0.15 + k0 * 0.15^2
  pvgt_h2_1 <- 0.15^2

  ## vgt : 11, 99
  pvgt_h1_2 <- 2 * k1 * 0.85 + k0 * 2 * 0.15 * 0.85
  pvgt_h2_2 <- 2 * 0.15 * 0.85

  ## rgt : 11, 11
  prgt <- 0.15^2

  ## Likelihood
  like_h1 <- pvgt_h1_1 * prgt + pvgt_h1_2 * prgt
  like_h2 <- pvgt_h2_1 * prgt + pvgt_h2_2 * prgt

  # Run
  likelihoods <- calc_kin_like_drop(vgt, rgt, af, af_al, pibd, myu, cons_mu, par_vic)

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
  myu <- 0.002
  cons_mu <- FALSE
  par_vic <- FALSE

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

  ## rgt : 11, 11
  prgt <- 0.15^2

  ## Likelihood
  like_h1 <- pvgt_h1_1 * prgt + (pvgt_h1_2 + pvgt_h1_3) * prgt
  like_h2 <- pvgt_h2_1 * prgt + (pvgt_h2_2 + pvgt_h2_3) * prgt

  # Run
  likelihoods <- calc_kin_like_drop(vgt, rgt, af, af_al, pibd, myu, cons_mu, par_vic)

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
  myu <- 0.002
  cons_mu <- FALSE
  par_vic <- FALSE

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

  ## rgt : 11, 13
  prgt <- 2 * 0.15 * 0.3

  ## Likelihood
  like_h1 <- pvgt_h1_1 * prgt + (pvgt_h1_2 + pvgt_h1_3) * prgt
  like_h2 <- pvgt_h2_1 * prgt + (pvgt_h2_2 + pvgt_h2_3) * prgt

  # Run
  likelihoods <- calc_kin_like_drop(vgt, rgt, af, af_al, pibd, myu, cons_mu, par_vic)

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
  myu <- 0.002
  cons_mu <- FALSE
  par_vic <- FALSE

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

  ## rgt : 11, 13
  prgt <- 2 * 0.15 * 0.3

  ## Likelihood
  like_h1 <- pvgt_h1_1 * prgt + (pvgt_h1_2 + pvgt_h1_3) * prgt
  like_h2 <- pvgt_h2_1 * prgt + (pvgt_h2_2 + pvgt_h2_3) * prgt

  # Run
  likelihoods <- calc_kin_like_drop(vgt, rgt, af, af_al, pibd, myu, cons_mu, par_vic)

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
  myu <- 0.002
  cons_mu <- FALSE
  par_vic <- FALSE

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

  ## rgt : 12, 13
  prgt <- 2 * 0.25 * 0.3

  ## Likelihood
  like_h1 <- pvgt_h1_1 * prgt + (pvgt_h1_2 + pvgt_h1_3 + pvgt_h1_4) * prgt
  like_h2 <- pvgt_h2_1 * prgt + (pvgt_h2_2 + pvgt_h2_3 + pvgt_h2_4) * prgt

  # Run
  likelihoods <- calc_kin_like_drop(vgt, rgt, af, af_al, pibd, myu, cons_mu, par_vic)

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
  myu <- 0.002
  cons_mu <- FALSE
  par_vic <- FALSE

  # True answer

  ## vgt : 11, 11, rgt : 11, 11
  pvgt_h1_1 <- k2 + 2 * k1 * 0.15 + k0 * 0.15^2
  pvgt_h2_1 <- 0.15^2
  prgt_1 <- 0.15^2

  ## vgt : 11, 11, rgt : 11, 99
  pvgt_h1_2 <- k1 * 0.15 + k0 * 0.15^2
  pvgt_h2_2 <- 0.15^2
  prgt_2 <- 2 * 0.15 * 0.85

  ## Likelihood
  like_h1 <- pvgt_h1_1 * prgt_1 + pvgt_h1_2 * prgt_2
  like_h2 <- pvgt_h2_1 * prgt_1 + pvgt_h2_2 * prgt_2

  # Run
  likelihoods <- calc_kin_like_drop(vgt, rgt, af, af_al, pibd, myu, cons_mu, par_vic)

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
  myu <- 0.002
  cons_mu <- FALSE
  par_vic <- FALSE

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

  ## Likelihood
  like_h1 <- pvgt_h1_1 * prgt_1 + pvgt_h1_2 * prgt_2 + pvgt_h1_3 * prgt_3
  like_h2 <- pvgt_h2_1 * prgt_1 + pvgt_h2_2 * prgt_2 + pvgt_h2_3 * prgt_3

  # Run
  likelihoods <- calc_kin_like_drop(vgt, rgt, af, af_al, pibd, myu, cons_mu, par_vic)

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
  myu <- 0.002
  cons_mu <- FALSE
  par_vic <- FALSE

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

  ## Likelihood
  like_h1 <- pvgt_h1_1 * prgt_1 + pvgt_h1_2 * prgt_2 + pvgt_h1_3 * prgt_3
  like_h2 <- pvgt_h2_1 * prgt_1 + pvgt_h2_2 * prgt_2 + pvgt_h2_3 * prgt_3

  # Run
  likelihoods <- calc_kin_like_drop(vgt, rgt, af, af_al, pibd, myu, cons_mu, par_vic)

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
  myu <- 0.002
  cons_mu <- FALSE
  par_vic <- FALSE

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

  ## Likelihood
  like_h1 <- pvgt_h1_1 * prgt_1 + pvgt_h1_2 * prgt_2 + pvgt_h1_3 * prgt_3
  like_h2 <- pvgt_h2_1 * prgt_1 + pvgt_h2_2 * prgt_2 + pvgt_h2_3 * prgt_3

  # Run
  likelihoods <- calc_kin_like_drop(vgt, rgt, af, af_al, pibd, myu, cons_mu, par_vic)

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
  myu <- 0.002
  cons_mu <- FALSE
  par_vic <- FALSE

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

  ## Likelihood
  like_h1 <- pvgt_h1_1 * prgt_1 + pvgt_h1_2 * prgt_2 + pvgt_h1_3 * prgt_3 + pvgt_h1_4 * prgt_4
  like_h2 <- pvgt_h2_1 * prgt_1 + pvgt_h2_2 * prgt_2 + pvgt_h2_3 * prgt_3 + pvgt_h2_4 * prgt_4

  # Run
  likelihoods <- calc_kin_like_drop(vgt, rgt, af, af_al, pibd, myu, cons_mu, par_vic)

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
  myu <- 0.002
  cons_mu <- FALSE
  par_vic <- FALSE

  # True answer

  ## vgt : 11, 11
  pvgt_h1_1 <- k2 + 2 * k1 * 0.15 + k0 * 0.15^2
  pvgt_h2_1 <- 0.15^2

  ## rgt : 11, 11
  prgt <- 0.15^2

  ## Likelihood
  like_h1 <- pvgt_h1_1 * prgt
  like_h2 <- pvgt_h2_1 * prgt

  # Run
  likelihoods <- calc_kin_like_drop(vgt, rgt, af, af_al, pibd, myu, cons_mu, par_vic)

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
  myu <- 0.002
  cons_mu <- FALSE
  par_vic <- FALSE

  # True answer

  ## vgt : 11, 11
  pvgt_h1_1 <- k1 * 0.15 + k0 * 0.15^2
  pvgt_h2_1 <- 0.15^2

  ## rgt : 11, 12
  prgt <- 2 * 0.15 * 0.25

  ## Likelihood
  like_h1 <- pvgt_h1_1 * prgt
  like_h2 <- pvgt_h2_1 * prgt

  # Run
  likelihoods <- calc_kin_like_drop(vgt, rgt, af, af_al, pibd, myu, cons_mu, par_vic)

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
  myu <- 0.002
  cons_mu <- FALSE
  par_vic <- FALSE

  # True answer

  ## vgt : 11, 12
  pvgt_h1_1 <- k2 + k1 * 0.15 + k1 * 0.25 + k0 * 2 * 0.15 * 0.25
  pvgt_h2_1 <- 2 * 0.15 * 0.25

  ## rgt : 11, 12
  prgt <- 2 * 0.15 * 0.25

  ## Likelihood
  like_h1 <- pvgt_h1_1 * prgt
  like_h2 <- pvgt_h2_1 * prgt

  # Run
  likelihoods <- calc_kin_like_drop(vgt, rgt, af, af_al, pibd, myu, cons_mu, par_vic)

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
  myu <- 0.002
  cons_mu <- FALSE
  par_vic <- FALSE

  # Run
  likelihoods_1 <- calc_kin_like_drop(vgt, rgt, af, af_al, pibd, myu, cons_mu, par_vic)
  likelihoods_2 <- calc_kin_like(vgt, rgt, af, af_al, pibd, myu, cons_mu, par_vic)

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

  # Run
  likelihoods_1 <- calc_kin_like_drop(vgt, rgt, af, af_al, pibd, myu, cons_mu, par_vic)
  likelihoods_2 <- calc_kin_like(vgt, rgt, af, af_al, pibd, myu, cons_mu, par_vic)

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
  myu <- 0.002
  cons_mu <- FALSE
  par_vic <- FALSE

  # Run
  likelihoods_1 <- calc_kin_like_drop(vgt, rgt, af, af_al, pibd, myu, cons_mu, par_vic)
  likelihoods_2 <- calc_kin_like(vgt, rgt, af, af_al, pibd, myu, cons_mu, par_vic)

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
  myu <- 0.002
  cons_mu <- TRUE
  par_vic <- TRUE

  # True answer

  ## vgt : 11, 11
  pvgt_h1_1 <- 0.15^2
  pvgt_h2_1 <- 0.15^2

  ## vgt : 11, 12
  pvgt_h1_2 <- 2 * 0.15 * 0.25
  pvgt_h2_2 <- 2 * 0.15 * 0.25

  ## vgt : 11, 99
  pvgt_h1_3 <- 2 * 0.15 * 0.6
  pvgt_h2_3 <- 2 * 0.15 * 0.6

  ## rgt : 12, 12
  prgt_h1_1 <- myu * 0.25
  prgt_h1_2 <- k1 * 0.25 + k0 * 0.25^2
  prgt_h1_3 <- myu * 0.25
  prgt_h2 <- 0.25^2

  ## Likelihood
  like_h1 <- pvgt_h1_1 * prgt_h1_1 + pvgt_h1_2 * prgt_h1_2 + pvgt_h1_3 * prgt_h1_3
  like_h2 <- (pvgt_h2_1 + pvgt_h2_2 + pvgt_h2_3) * prgt_h2

  # Run
  likelihoods <- calc_kin_like_drop(vgt, rgt, af, af_al, pibd, myu, cons_mu, par_vic)

  # Test
  expect_equal(as.numeric(likelihoods[1]), like_h1)
  expect_equal(as.numeric(likelihoods[2]), like_h2)
})

test_that("calc_kin_like_drop mutation pattern 2", {

  # Condition
  vgt <- 11
  rgt <- c(12, 12)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  af_al <- 11:15
  k2 <- 0
  k1 <- 0.5
  k0 <- 0
  pibd <- c(k2, 2 * k1, k0)
  myu <- 0.002
  cons_mu <- TRUE
  par_vic <- FALSE

  # True answer

  ## vgt : 11, 11
  pvgt_h1_1 <- myu * 0.15
  pvgt_h2_1 <- 0.15^2

  ## vgt : 11, 12
  pvgt_h1_2 <- 2 * k1 * 0.15 + k0 * 2 * 0.15 * 0.25
  pvgt_h2_2 <- 2 * 0.15 * 0.25

  ## vgt : 11, 99
  pvgt_h1_3 <- myu * (0.15 + 0.6)
  pvgt_h2_3 <- 2 * 0.15 * 0.6

  ## rgt : 12, 12
  prgt <- 0.25^2

  ## Likelihood
  like_h1 <- pvgt_h1_1 * prgt + (pvgt_h1_2 + pvgt_h1_3) * prgt
  like_h2 <- pvgt_h2_1 * prgt + (pvgt_h2_2 + pvgt_h2_3) * prgt

  # Run
  likelihoods <- calc_kin_like_drop(vgt, rgt, af, af_al, pibd, myu, cons_mu, par_vic)

  # Test
  expect_equal(as.numeric(likelihoods[1]), like_h1)
  expect_equal(as.numeric(likelihoods[2]), like_h2)
})
