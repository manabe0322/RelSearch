test_that("calc_kin_like_pc without mutation pattern 1", {
  # Condition
  vgt <- c(11, 11)
  rgt <- c(11, 11)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  af_al <- 11:15
  pibd <- c(0, 1, 0)

  # Run
  al_prob <- extract_al_prob(vgt, rgt, af, af_al)
  myu_per_inheritance <- rep(1, 4)
  likelihoods_pc <- calc_kin_like_pc(vgt, rgt, al_prob, myu_per_inheritance)
  likelihoods_gen <- calc_kin_like(vgt, rgt, al_prob, pibd)

  # Test
  expect_equal(likelihoods_pc[1], likelihoods_gen[1])
  expect_equal(likelihoods_pc[2], likelihoods_gen[2])
})

test_that("calc_kin_like_pc without mutation pattern 2", {
  # Condition
  vgt <- c(11, 12)
  rgt <- c(11, 12)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  af_al <- 11:15
  pibd <- c(0, 1, 0)

  # Run
  al_prob <- extract_al_prob(vgt, rgt, af, af_al)
  myu_per_inheritance <- c(1, 0, 0, 1)
  likelihoods_pc <- calc_kin_like_pc(vgt, rgt, al_prob, myu_per_inheritance)
  likelihoods_gen <- calc_kin_like(vgt, rgt, al_prob, pibd)

  # Test
  expect_equal(likelihoods_pc[1], likelihoods_gen[1])
  expect_equal(likelihoods_pc[2], likelihoods_gen[2])
})

test_that("calc_kin_like_pc without mutation pattern 3", {
  # Condition
  vgt <- c(11, 12)
  rgt <- c(11, 11)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  af_al <- 11:15
  pibd <- c(0, 1, 0)

  # Run
  al_prob <- extract_al_prob(vgt, rgt, af, af_al)
  myu_per_inheritance <- c(1, 1, 0, 0)
  likelihoods_pc <- calc_kin_like_pc(vgt, rgt, al_prob, myu_per_inheritance)
  likelihoods_gen <- calc_kin_like(vgt, rgt, al_prob, pibd)

  # Test
  expect_equal(likelihoods_pc[1], likelihoods_gen[1])
  expect_equal(likelihoods_pc[2], likelihoods_gen[2])
})

test_that("calc_kin_like_pc without mutation pattern 4", {
  # Condition
  vgt <- c(11, 11)
  rgt <- c(11, 12)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  af_al <- 11:15
  pibd <- c(0, 1, 0)

  # Run
  al_prob <- extract_al_prob(vgt, rgt, af, af_al)
  myu_per_inheritance <- c(1, 0, 1, 0)
  likelihoods_pc <- calc_kin_like_pc(vgt, rgt, al_prob, myu_per_inheritance)
  likelihoods_gen <- calc_kin_like(vgt, rgt, al_prob, pibd)

  # Test
  expect_equal(likelihoods_pc[1], likelihoods_gen[1])
  expect_equal(likelihoods_pc[2], likelihoods_gen[2])
})

test_that("calc_kin_like_pc without mutation pattern 5", {
  # Condition
  vgt <- c(11, 12)
  rgt <- c(12, 13)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  af_al <- 11:15
  pibd <- c(0, 1, 0)

  # Run
  al_prob <- extract_al_prob(vgt, rgt, af, af_al)
  myu_per_inheritance <- c(0, 0, 1, 0)
  likelihoods_pc <- calc_kin_like_pc(vgt, rgt, al_prob, myu_per_inheritance)
  likelihoods_gen <- calc_kin_like(vgt, rgt, al_prob, pibd)

  # Test
  expect_equal(likelihoods_pc[1], likelihoods_gen[1])
  expect_equal(likelihoods_pc[2], likelihoods_gen[2])
})

test_that("calc_kin_like_pc without mutation pattern 6", {
  # Condition
  vgt <- c(11, 12)
  rgt <- c(13, 14)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  af_al <- 11:15
  pibd <- c(0, 1, 0)

  # Run
  al_prob <- extract_al_prob(vgt, rgt, af, af_al)
  myu_per_inheritance <- c(0, 0, 0, 0)
  likelihoods_pc <- calc_kin_like_pc(vgt, rgt, al_prob, myu_per_inheritance)
  likelihoods_gen <- calc_kin_like(vgt, rgt, al_prob, pibd)

  # Test
  expect_equal(likelihoods_pc[1], likelihoods_gen[1])
  expect_equal(likelihoods_pc[2], likelihoods_gen[2])
})

test_that("calc_kin_like_pc mutation pattern 1", {
  # Condition
  vgt <- c(11, 11)
  rgt <- c(12, 12)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  af_al <- 11:15
  myu_pat_m2 <- 0.001
  myu_pat_m1 <- 0.01
  myu_pat_p1 <- 0.02
  myu_pat_p2 <- 0.002
  myu_pat_0 <- 1 - myu_pat_m2 - myu_pat_m1 - myu_pat_p1 - myu_pat_p2
  myu_mat_m2 <- 0.00025
  myu_mat_m1 <- 0.0025
  myu_mat_p1 <- 0.005
  myu_mat_p2 <- 0.0005
  myu_mat_0 <- 1 - myu_mat_m2 - myu_mat_m1 - myu_mat_p1 - myu_mat_p2
  bool_parent_male <- TRUE

  # Run
  al_prob <- extract_al_prob(vgt, rgt, af, af_al)
  myu_per_inheritance <- set_myu_per_inheritance(vgt, rgt,
                                                 myu_pat_m2, myu_pat_m1, myu_pat_0, myu_pat_p1, myu_pat_p2,
                                                 myu_mat_m2, myu_mat_m1, myu_mat_0, myu_mat_p1, myu_mat_p2,
                                                 bool_parent_male)
  likelihoods_pc <- calc_kin_like_pc(vgt, rgt, al_prob, myu_per_inheritance)

  # Test
  expect_equal(likelihoods_pc[1], 0.15^2 * myu_pat_p1 * 0.25)
  expect_equal(likelihoods_pc[2], 0.15^2 * 0.25^2)
})

test_that("calc_kin_like mutation pattern 2", {
  # Condition
  vgt <- c(11, 11)
  rgt <- c(12, 12)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  af_al <- 11:15
  myu_pat_m2 <- 0.001
  myu_pat_m1 <- 0.01
  myu_pat_p1 <- 0.02
  myu_pat_p2 <- 0.002
  myu_pat_0 <- 1 - myu_pat_m2 - myu_pat_m1 - myu_pat_p1 - myu_pat_p2
  myu_mat_m2 <- 0.00025
  myu_mat_m1 <- 0.0025
  myu_mat_p1 <- 0.005
  myu_mat_p2 <- 0.0005
  myu_mat_0 <- 1 - myu_mat_m2 - myu_mat_m1 - myu_mat_p1 - myu_mat_p2
  bool_parent_male <- FALSE

  # Run
  al_prob <- extract_al_prob(vgt, rgt, af, af_al)
  myu_per_inheritance <- set_myu_per_inheritance(vgt, rgt,
                                                 myu_pat_m2, myu_pat_m1, myu_pat_0, myu_pat_p1, myu_pat_p2,
                                                 myu_mat_m2, myu_mat_m1, myu_mat_0, myu_mat_p1, myu_mat_p2,
                                                 bool_parent_male)
  likelihoods_pc <- calc_kin_like_pc(vgt, rgt, al_prob, myu_per_inheritance)

  # Test
  expect_equal(likelihoods_pc[1], 0.15^2 * myu_mat_p1 * 0.25)
  expect_equal(likelihoods_pc[2], 0.15^2 * 0.25^2)
})

test_that("calc_kin_like_pc mutation pattern 3", {
  # Condition
  vgt <- c(11, 12)
  rgt <- c(13, 14)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  af_al <- 11:15
  myu_pat_m2 <- 0.001
  myu_pat_m1 <- 0.01
  myu_pat_p1 <- 0.02
  myu_pat_p2 <- 0.002
  myu_pat_0 <- 1 - myu_pat_m2 - myu_pat_m1 - myu_pat_p1 - myu_pat_p2
  myu_mat_m2 <- 0.00025
  myu_mat_m1 <- 0.0025
  myu_mat_p1 <- 0.005
  myu_mat_p2 <- 0.0005
  myu_mat_0 <- 1 - myu_mat_m2 - myu_mat_m1 - myu_mat_p1 - myu_mat_p2
  bool_parent_male <- TRUE

  # Run
  al_prob <- extract_al_prob(vgt, rgt, af, af_al)
  myu_per_inheritance <- set_myu_per_inheritance(vgt, rgt,
                                                 myu_pat_m2, myu_pat_m1, myu_pat_0, myu_pat_p1, myu_pat_p2,
                                                 myu_mat_m2, myu_mat_m1, myu_mat_0, myu_mat_p1, myu_mat_p2,
                                                 bool_parent_male)
  likelihoods_pc <- calc_kin_like_pc(vgt, rgt, al_prob, myu_per_inheritance)

  # Test
  expect_equal(likelihoods_pc[1], 0.15 * 0.25 * (myu_pat_p2 * 0.25 + myu_pat_p1 * 0.25 + myu_pat_p2 * 0.3))
  expect_equal(likelihoods_pc[2], 2 * 0.15 * 0.25 * 2 * 0.3 * 0.25)
})

test_that("calc_kin_like_pc mutation pattern 4", {
  # Condition
  vgt <- c(11, 12)
  rgt <- c(13, 14)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  af_al <- 11:15
  myu_pat_m2 <- 0.001
  myu_pat_m1 <- 0.01
  myu_pat_p1 <- 0.02
  myu_pat_p2 <- 0.002
  myu_pat_0 <- 1 - myu_pat_m2 - myu_pat_m1 - myu_pat_p1 - myu_pat_p2
  myu_mat_m2 <- 0.00025
  myu_mat_m1 <- 0.0025
  myu_mat_p1 <- 0.005
  myu_mat_p2 <- 0.0005
  myu_mat_0 <- 1 - myu_mat_m2 - myu_mat_m1 - myu_mat_p1 - myu_mat_p2
  bool_parent_male <- FALSE

  # Run
  al_prob <- extract_al_prob(vgt, rgt, af, af_al)
  myu_per_inheritance <- set_myu_per_inheritance(vgt, rgt,
                                                 myu_pat_m2, myu_pat_m1, myu_pat_0, myu_pat_p1, myu_pat_p2,
                                                 myu_mat_m2, myu_mat_m1, myu_mat_0, myu_mat_p1, myu_mat_p2,
                                                 bool_parent_male)
  likelihoods_pc <- calc_kin_like_pc(vgt, rgt, al_prob, myu_per_inheritance)

  # Test
  expect_equal(likelihoods_pc[1], 0.15 * 0.25 * (myu_mat_p2 * 0.25 + myu_mat_p1 * 0.25 + myu_mat_p2 * 0.3))
  expect_equal(likelihoods_pc[2], 2 * 0.15 * 0.25 * 2 * 0.3 * 0.25)
})

test_that("calc_kin_like_pc mutation pattern 5", {
  # Condition
  vgt <- c(13, 14)
  rgt <- c(11, 12)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  af_al <- 11:15
  myu_pat_m2 <- 0.001
  myu_pat_m1 <- 0.01
  myu_pat_p1 <- 0.02
  myu_pat_p2 <- 0.002
  myu_pat_0 <- 1 - myu_pat_m2 - myu_pat_m1 - myu_pat_p1 - myu_pat_p2
  myu_mat_m2 <- 0.00025
  myu_mat_m1 <- 0.0025
  myu_mat_p1 <- 0.005
  myu_mat_p2 <- 0.0005
  myu_mat_0 <- 1 - myu_mat_m2 - myu_mat_m1 - myu_mat_p1 - myu_mat_p2
  bool_parent_male <- TRUE

  # Run
  al_prob <- extract_al_prob(vgt, rgt, af, af_al)
  myu_per_inheritance <- set_myu_per_inheritance(vgt, rgt,
                                                 myu_pat_m2, myu_pat_m1, myu_pat_0, myu_pat_p1, myu_pat_p2,
                                                 myu_mat_m2, myu_mat_m1, myu_mat_0, myu_mat_p1, myu_mat_p2,
                                                 bool_parent_male)
  likelihoods_pc <- calc_kin_like_pc(vgt, rgt, al_prob, myu_per_inheritance)

  # Test
  expect_equal(likelihoods_pc[1], 0.3 * 0.25 * (myu_pat_m2 * 0.25 + myu_pat_m1 * 0.15 + myu_pat_m2 * 0.15))
  expect_equal(likelihoods_pc[2], 2 * 0.15 * 0.25 * 2 * 0.3 * 0.25)
})

test_that("calc_kin_like_pc mutation pattern 6", {
  # Condition
  vgt <- c(13, 14)
  rgt <- c(11, 12)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  af_al <- 11:15
  myu_pat_m2 <- 0.001
  myu_pat_m1 <- 0.01
  myu_pat_p1 <- 0.02
  myu_pat_p2 <- 0.002
  myu_pat_0 <- 1 - myu_pat_m2 - myu_pat_m1 - myu_pat_p1 - myu_pat_p2
  myu_mat_m2 <- 0.00025
  myu_mat_m1 <- 0.0025
  myu_mat_p1 <- 0.005
  myu_mat_p2 <- 0.0005
  myu_mat_0 <- 1 - myu_mat_m2 - myu_mat_m1 - myu_mat_p1 - myu_mat_p2
  bool_parent_male <- FALSE

  # Run
  al_prob <- extract_al_prob(vgt, rgt, af, af_al)
  myu_per_inheritance <- set_myu_per_inheritance(vgt, rgt,
                                                 myu_pat_m2, myu_pat_m1, myu_pat_0, myu_pat_p1, myu_pat_p2,
                                                 myu_mat_m2, myu_mat_m1, myu_mat_0, myu_mat_p1, myu_mat_p2,
                                                 bool_parent_male)
  likelihoods_pc <- calc_kin_like_pc(vgt, rgt, al_prob, myu_per_inheritance)

  # Test
  expect_equal(likelihoods_pc[1], 0.3 * 0.25 * (myu_mat_m2 * 0.25 + myu_mat_m1 * 0.15 + myu_mat_m2 * 0.15))
  expect_equal(likelihoods_pc[2], 2 * 0.15 * 0.25 * 2 * 0.3 * 0.25)
})

test_that("calc_kin_like_pc mutation pattern 7", {
  # Condition
  vgt <- c(11.2, 13.3)
  rgt <- c(12.3, 13.3)
  af <- c(0.15, 0.2, 0.25, 0.35, 0.05)
  af_al <- c(11.2, 12.3, 13.2, 13.3, 14.3)
  myu_pat_m2 <- 0.001
  myu_pat_m1 <- 0.01
  myu_pat_p1 <- 0.02
  myu_pat_p2 <- 0.002
  myu_pat_0 <- 1 - myu_pat_m2 - myu_pat_m1 - myu_pat_p1 - myu_pat_p2
  myu_mat_m2 <- 0.00025
  myu_mat_m1 <- 0.0025
  myu_mat_p1 <- 0.005
  myu_mat_p2 <- 0.0005
  myu_mat_0 <- 1 - myu_mat_m2 - myu_mat_m1 - myu_mat_p1 - myu_mat_p2
  bool_parent_male <- TRUE

  # Run
  al_prob <- extract_al_prob(vgt, rgt, af, af_al)
  myu_per_inheritance <- set_myu_per_inheritance(vgt, rgt,
                                                 myu_pat_m2, myu_pat_m1, myu_pat_0, myu_pat_p1, myu_pat_p2,
                                                 myu_mat_m2, myu_mat_m1, myu_mat_0, myu_mat_p1, myu_mat_p2,
                                                 bool_parent_male)
  likelihoods_pc <- calc_kin_like_pc(vgt, rgt, al_prob, myu_per_inheritance)

  # Test
  expect_equal(likelihoods_pc[1], 0.15 * 0.35 * (myu_pat_m1 * 0.35 + myu_pat_0 * 0.2))
  expect_equal(likelihoods_pc[2], 2 * 0.15 * 0.35 * 2 * 0.2 * 0.35)
})

test_that("calc_kin_like_pc mutation pattern 7", {
  # Condition
  vgt <- c(11.2, 13.3)
  rgt <- c(12.3, 12.3)
  af <- c(0.15, 0.2, 0.25, 0.35, 0.05)
  af_al <- c(11.2, 12.3, 13.2, 13.3, 14.3)
  myu_pat_m2 <- 0.001
  myu_pat_m1 <- 0.01
  myu_pat_p1 <- 0.02
  myu_pat_p2 <- 0.002
  myu_pat_0 <- 1 - myu_pat_m2 - myu_pat_m1 - myu_pat_p1 - myu_pat_p2
  myu_mat_m2 <- 0.00025
  myu_mat_m1 <- 0.0025
  myu_mat_p1 <- 0.005
  myu_mat_p2 <- 0.0005
  myu_mat_0 <- 1 - myu_mat_m2 - myu_mat_m1 - myu_mat_p1 - myu_mat_p2
  bool_parent_male <- TRUE

  # Run
  al_prob <- extract_al_prob(vgt, rgt, af, af_al)
  myu_per_inheritance <- set_myu_per_inheritance(vgt, rgt,
                                                 myu_pat_m2, myu_pat_m1, myu_pat_0, myu_pat_p1, myu_pat_p2,
                                                 myu_mat_m2, myu_mat_m1, myu_mat_0, myu_mat_p1, myu_mat_p2,
                                                 bool_parent_male)
  likelihoods_pc <- calc_kin_like_pc(vgt, rgt, al_prob, myu_per_inheritance)

  # Test
  expect_equal(likelihoods_pc[1], 0.15 * 0.35 * (myu_pat_m1 * 0.2))
  expect_equal(likelihoods_pc[2], 2 * 0.15 * 0.35 * 0.2^2)
})
