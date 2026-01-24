# Extracted from test_calc_kin_like_drop.R:661

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "RelSearch", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
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
pvgt_h1_1 <- 0.15^2
pvgt_h2_1 <- 0.15^2
pvgt_h1_2 <- 2 * 0.15 * 0.25
pvgt_h2_2 <- 2 * 0.15 * 0.25
pvgt_h1_3 <- 2 * 0.15 * 0.6
pvgt_h2_3 <- 2 * 0.15 * 0.6
prgt_h1_1 <- myu * 0.25
prgt_h1_2 <- k1 * 0.25 + k0 * 0.25^2
prgt_h1_3 <- myu * 0.25
prgt_h2 <- 0.25^2
like_h1 <- pvgt_h1_1 * prgt_h1_1 + pvgt_h1_2 * prgt_h1_2 + pvgt_h1_3 * prgt_h1_3
like_h2 <- (pvgt_h2_1 + pvgt_h2_2 + pvgt_h2_3) * prgt_h2
likelihoods <- calc_kin_like_drop(vgt, rgt, af, af_al, pibd, myu, cons_mu, par_vic)
