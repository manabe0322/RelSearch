test_that("setdiff_string pattern 1", {
  # Condition
  v_hap <- "73N 152N 164N 200N 235N 263N 16223N 16254N 16290N 16319N 16362N"
  v_hap <- strsplit(v_hap, " ")[[1]]
  r_hap <- "73N 152N 164N 200N 235N 263N 16223N 16254N 16290N 16319N 16362N"
  r_hap <- strsplit(r_hap, " ")[[1]]

  # Run
  ans1 <- setdiff_string(v_hap, r_hap)
  ans2 <- setdiff_string(r_hap, v_hap)

  # Test
  expect_equal(ans1, character(0))
  expect_equal(ans2, character(0))
})

test_that("setdiff_string pattern 2", {
  # Condition
  v_hap <- "73N 152N 164N 200N 235N 263N 292N 16223N 16254N 16290N 16319N 16362N"
  v_hap <- strsplit(v_hap, " ")[[1]]
  r_hap <- "73N 152N 164N 200N 235N 263N 16223N 16254N 16290N 16319N 16362N"
  r_hap <- strsplit(r_hap, " ")[[1]]

  # Run
  ans1 <- setdiff_string(v_hap, r_hap)
  ans2 <- setdiff_string(r_hap, v_hap)

  # Test
  expect_equal(length(ans1), 1)
  expect_equal(ans1, "292N")
  expect_equal(ans2, character(0))
})

test_that("setdiff_string pattern 3", {
  # Condition
  v_hap <- "73N 152N 164N 200N 235N 263N 292N 16223N 16254N 16290N 16319N 16362N"
  v_hap <- strsplit(v_hap, " ")[[1]]
  r_hap <- "73N 152N 164N 200N 235N 263N 293N 16223N 16254N 16290N 16319N 16362N"
  r_hap <- strsplit(r_hap, " ")[[1]]

  # Run
  ans1 <- setdiff_string(v_hap, r_hap)
  ans2 <- setdiff_string(r_hap, v_hap)

  # Test
  expect_equal(length(ans1), 1)
  expect_equal(ans1, "292N")
  expect_equal(length(ans2), 1)
  expect_equal(ans2, "293N")
})

test_that("setdiff_string pattern 4", {
  # Condition
  v_hap <- "73N 152N"
  v_hap <- strsplit(v_hap, " ")[[1]]
  r_hap <- "16319N 16362N"
  r_hap <- strsplit(r_hap, " ")[[1]]

  # Run
  ans1 <- setdiff_string(v_hap, r_hap)
  ans2 <- setdiff_string(r_hap, v_hap)

  # Test
  expect_equal(length(ans1), 2)
  expect_equal(ans1[1], "73N")
  expect_equal(ans1[2], "152N")
  expect_equal(length(ans2), 2)
  expect_equal(ans2[1], "16319N")
  expect_equal(ans2[2], "16362N")
})
