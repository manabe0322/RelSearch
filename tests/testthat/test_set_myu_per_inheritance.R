test_that("set_myu_per_inheritance pattern1", {
  # Condition
  vgt <- c(11, 13)
  rgt <- c(10, 11)
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
  myu_per_inheritance <- set_myu_per_inheritance(vgt, rgt,
                                                 myu_pat_m2, myu_pat_m1, myu_pat_0, myu_pat_p1, myu_pat_p2,
                                                 myu_mat_m2, myu_mat_m1, myu_mat_0, myu_mat_p1, myu_mat_p2,
                                                 bool_parent_male)

  # Test
  expect_equal(length(myu_per_inheritance), 4)
  expect_equal(myu_per_inheritance[1], myu_pat_m1)
  expect_equal(myu_per_inheritance[2], 1 - myu_pat_m2 - myu_pat_m1 - myu_pat_p1 - myu_pat_p2)
  expect_equal(myu_per_inheritance[3], 0)
  expect_equal(myu_per_inheritance[4], myu_pat_m2)
})

test_that("set_myu_per_inheritance pattern2", {
  # Condition
  vgt <- c(11, 13)
  rgt <- c(10, 11)
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
  myu_per_inheritance <- set_myu_per_inheritance(vgt, rgt,
                                                 myu_pat_m2, myu_pat_m1, myu_pat_0, myu_pat_p1, myu_pat_p2,
                                                 myu_mat_m2, myu_mat_m1, myu_mat_0, myu_mat_p1, myu_mat_p2,
                                                 bool_parent_male)

  # Test
  expect_equal(length(myu_per_inheritance), 4)
  expect_equal(myu_per_inheritance[1], myu_mat_m1)
  expect_equal(myu_per_inheritance[2], 1 - myu_mat_m2 - myu_mat_m1 - myu_mat_p1 - myu_mat_p2)
  expect_equal(myu_per_inheritance[3], 0)
  expect_equal(myu_per_inheritance[4], myu_mat_m2)
})

test_that("set_myu_per_inheritance pattern3", {
  # Condition
  vgt <- c(10, 11)
  rgt <- c(11, 13)
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
  myu_per_inheritance <- set_myu_per_inheritance(vgt, rgt,
                                                 myu_pat_m2, myu_pat_m1, myu_pat_0, myu_pat_p1, myu_pat_p2,
                                                 myu_mat_m2, myu_mat_m1, myu_mat_0, myu_mat_p1, myu_mat_p2,
                                                 bool_parent_male)

  # Test
  expect_equal(length(myu_per_inheritance), 4)
  expect_equal(myu_per_inheritance[1], myu_pat_p1)
  expect_equal(myu_per_inheritance[2], 0)
  expect_equal(myu_per_inheritance[3], 1 - myu_pat_m2 - myu_pat_m1 - myu_pat_p1 - myu_pat_p2)
  expect_equal(myu_per_inheritance[4], myu_pat_p2)
})
