test_that("union_vr_al pattern 1", {

  # Condition
  vgt <- c(11, 11)
  rgt <- c(11, 11)

  # Run
  uniq_vr_al <- union_vr_al(vgt, rgt)

  # Test
  expect_equal(length(uniq_vr_al), 1)
  expect_equal(uniq_vr_al[1], 11)
})

test_that("union_vr_al pattern 2", {

  # Condition
  vgt <- c(11, 11)
  rgt <- c(11, 12)

  # Run
  uniq_vr_al <- union_vr_al(vgt, rgt)

  # Test
  expect_equal(length(uniq_vr_al), 2)
  expect_equal(uniq_vr_al[1], 11)
  expect_equal(uniq_vr_al[2], 12)
})

test_that("union_vr_al pattern 3", {

  # Condition
  vgt <- c(11, 13)
  rgt <- c(12, 14)

  # Run
  uniq_vr_al <- union_vr_al(vgt, rgt)

  # Test
  expect_equal(length(uniq_vr_al), 4)
  expect_equal(uniq_vr_al[1], 11)
  expect_equal(uniq_vr_al[2], 12)
  expect_equal(uniq_vr_al[3], 13)
  expect_equal(uniq_vr_al[4], 14)
})

test_that("union_vr_al pattern 4", {

  # Condition
  vgt <- 11
  rgt <- 11

  # Run
  uniq_vr_al <- union_vr_al(vgt, rgt)

  # Test
  expect_equal(length(uniq_vr_al), 1)
  expect_equal(uniq_vr_al[1], 11)
})
