test_that("extract_pos_mt_vr pattern 1", {

  # Condition
  v_range <- "73-340 16024-16365"
  r_range <- "73-340 16024-16365"

  # Run
  pos_mt_vr <- extract_pos_mt_vr(v_range, r_range)

  # Test
  expect_equal(length(pos_mt_vr), 610)
  expect_equal(pos_mt_vr[1], 73)
  expect_equal(pos_mt_vr[268], 340)
  expect_equal(pos_mt_vr[269], 16024)
  expect_equal(pos_mt_vr[610], 16365)
})

test_that("extract_pos_mt_vr pattern 2-1", {

  # Condition
  v_range <- "72-339 16024-16365"
  r_range <- "73-340 16025-16364"

  # Run
  pos_mt_vr <- extract_pos_mt_vr(v_range, r_range)

  # Test
  expect_equal(length(pos_mt_vr), 607)
  expect_equal(pos_mt_vr[1], 73)
  expect_equal(pos_mt_vr[267], 339)
  expect_equal(pos_mt_vr[268], 16025)
  expect_equal(pos_mt_vr[607], 16364)
})

test_that("extract_pos_mt_vr pattern 2-2", {

  # Condition
  v_range <- "16024-16365 72-339"
  r_range <- "16025-16364 73-340"

  # Run
  pos_mt_vr <- extract_pos_mt_vr(v_range, r_range)

  # Test
  expect_equal(length(pos_mt_vr), 607)
  expect_equal(pos_mt_vr[1], 73)
  expect_equal(pos_mt_vr[267], 339)
  expect_equal(pos_mt_vr[268], 16025)
  expect_equal(pos_mt_vr[607], 16364)
})

test_that("extract_pos_mt_vr pattern 3", {

  # Condition
  v_range <- "73-340"
  r_range <- "16024-16365"

  # Run
  pos_mt_vr <- extract_pos_mt_vr(v_range, r_range)

  # Test
  expect_equal(pos_mt_vr, numeric(0))
})

test_that("extract_pos_mt_vr pattern 4", {

  # Condition
  v_range <- "16024-16173 16209-16365"
  r_range <- "16050-16250"

  # Run
  pos_mt_vr <- extract_pos_mt_vr(v_range, r_range)

  # Test
  expect_equal(length(pos_mt_vr), 166)
  expect_equal(pos_mt_vr[1], 16050)
  expect_equal(pos_mt_vr[124], 16173)
  expect_equal(pos_mt_vr[125], 16209)
  expect_equal(pos_mt_vr[166], 16250)
})
