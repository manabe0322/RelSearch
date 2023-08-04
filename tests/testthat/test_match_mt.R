test_that("match_mt pattern 1", {

  # Condition
  v_hap <- "16223T 16299G 16362C 73G 263G 315.1C"
  v_hap <- strsplit(v_hap, " ")[[1]]
  v_range <- "16024-16365 73-340"
  r_hap <- "16223T 16299G 16362C 73G 263G 315.1C"
  r_hap <- strsplit(r_hap, " ")[[1]]
  r_range <- "16024-16365 73-340"

  # Run
  result <- match_mt(v_hap, v_range, r_hap, r_range)

  # Test
  expect_equal(result[1], "0")
  expect_equal(result[2], "73-340 16024-16365")
  expect_equal(result[3], "610")
})

test_that("match_mt pattern 2", {

  # Condition
  v_hap <- "16092C 16209C 16223T 16324C 73G 263G 309.1C 309.2C 315.1C"
  v_hap <- strsplit(v_hap, " ")[[1]]
  v_range <- "16025-16364 73-340"
  r_hap <- "16209C 16223T 16291T 16324C 73G 263G 309.1C 315.1C"
  r_hap <- strsplit(r_hap, " ")[[1]]
  r_range <- "16024-16365 74-339"

  # Run
  result <- match_mt(v_hap, v_range, r_hap, r_range)

  # Test
  expect_equal(result[1], "3")
  expect_equal(result[2], "74-339 16025-16364")
  expect_equal(result[3], "606")
})

test_that("match_mt pattern 3", {

  # Condition
  v_hap <- "16092C 16209C 16223T 16324C 73G 263G 309.1C 309.2C 315.1C"
  v_hap <- strsplit(v_hap, " ")[[1]]
  v_range <- "16024-16365"
  r_hap <- "16209C 16223T 16291T 16324C 73G 263G 309.1C 315.1C"
  r_hap <- strsplit(r_hap, " ")[[1]]
  r_range <- "73-340"

  # Run
  result <- match_mt(v_hap, v_range, r_hap, r_range)

  # Test
  expect_equal(result[1], "")
  expect_equal(result[2], "")
  expect_equal(result[3], "0")
})

test_that("match_mt pattern 4", {

  # Condition
  v_hap <- ""
  v_range <- ""
  r_hap <- "16209C 16223T 16291T 16324C 73G 263G 309.1C 315.1C"
  r_hap <- strsplit(r_hap, " ")[[1]]
  r_range <- "73-340"

  # Run
  result <- match_mt(v_hap, v_range, r_hap, r_range)

  # Test
  expect_equal(result[1], "")
  expect_equal(result[2], "")
  expect_equal(result[3], "0")
})

test_that("match_mt pattern 5", {

  # Condition
  v_hap <- "73G"
  v_range <- "73"
  r_hap <- "16209C 16223T 16291T 16324C 73G 263G 309.1C 315.1C"
  r_hap <- strsplit(r_hap, " ")[[1]]
  r_range <- "73-340"

  # Run
  result <- match_mt(v_hap, v_range, r_hap, r_range)

  # Test
  expect_equal(result[1], "0")
  expect_equal(result[2], "73")
  expect_equal(result[3], "1")
})
