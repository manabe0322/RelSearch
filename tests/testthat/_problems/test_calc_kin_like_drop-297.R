# Extracted from test_calc_kin_like_drop.R:297

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "RelSearch", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
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
pvgt_h1_1 <- k0 * 0.15^2
pvgt_h2_1 <- 0.15^2
prgt_1 <- 0.25^2
pvgt_h1_2 <- k1 * 0.15 + k0 * 0.15^2
pvgt_h2_2 <- 0.15^2
prgt_2 <- 2 * 0.15 * 0.25
pvgt_h1_3 <- k0 * 0.15^2
pvgt_h2_3 <- 0.15^2
prgt_3 <- 2 * 0.25 * 0.6
like_h1 <- pvgt_h1_1 * prgt_1 + pvgt_h1_2 * prgt_2 + pvgt_h1_3 * prgt_3
like_h2 <- pvgt_h2_1 * prgt_1 + pvgt_h2_2 * prgt_2 + pvgt_h2_3 * prgt_3
likelihoods <- calc_kin_like_drop(vgt, rgt, af, af_al, pibd, myu, cons_mu, par_vic)
