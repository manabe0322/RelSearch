test_that("make_dummy_af", {

  # Condition
  uniq_vr_al <- c(11, 12)
  af <- c(0.15, 0.25, 0.3, 0.25, 0.05)
  af_al <- 11:15

  # Run
  dummy_af_data <- make_dummy_af(uniq_vr_al, af, af_al)
  af_dummy <- dummy_af_data[[1]]
  af_al_dummy <- dummy_af_data[[2]]

  # Test
  expect_equal(length(af_dummy), 3)
  expect_equal(af_dummy[1], 0.15)
  expect_equal(af_dummy[2], 0.25)
  expect_equal(af_dummy[3], 0.6)
  expect_equal(length(af_al_dummy), 3)
  expect_equal(af_al_dummy[1], 11)
  expect_equal(af_al_dummy[2], 12)
  expect_equal(af_al_dummy[3], 99)
})
