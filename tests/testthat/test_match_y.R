test_that("match_y pattern 1", {

  # Condition
  v_hap <- c("10", "15, 16", "20")
  r_hap <- c("10", "15, 16", "20")

  # Run
  result <- match_y(v_hap, r_hap)

  # Test
  expect_equal(length(result), 3)
  expect_equal(length(result[[1]]), 4)
  expect_equal(result[[1]][1], 0)
  expect_equal(result[[1]][2], 0)
  expect_equal(result[[1]][3], 0)
  expect_equal(result[[1]][4], 0)
  expect_equal(result[[2]][1], 0)
  expect_equal(result[[2]][2], 0)
  expect_equal(result[[2]][3], 0)
  expect_equal(result[[2]][4], 0)
  expect_equal(result[[3]][1], 0)
  expect_equal(result[[3]][2], 0)
  expect_equal(result[[3]][3], 0)
  expect_equal(result[[3]][4], 0)
})

test_that("match_y pattern 2", {

  # Condition
  v_hap <- c("10", "15, 16", "")
  r_hap <- c("10", "15, 17", "22")

  # Run
  result <- match_y(v_hap, r_hap)

  # Test
  expect_equal(length(result), 3)
  expect_equal(length(result[[1]]), 4)
  expect_equal(result[[1]][1], 0)
  expect_equal(result[[1]][2], 1)
  expect_equal(result[[1]][3], 0)
  expect_equal(result[[1]][4], 1)
  expect_equal(result[[2]][1], 0)
  expect_equal(result[[2]][2], 0)
  expect_equal(result[[2]][3], 1)
  expect_equal(result[[2]][4], 1)
  expect_equal(result[[3]][1], 0)
  expect_equal(result[[3]][2], 1)
  expect_equal(result[[3]][3], 0)
  expect_equal(result[[3]][4], 1)
})

test_that("match_y pattern 3", {

  # Condition
  v_hap <- c("10", "15", "20")
  r_hap <- c("10", "15, 17", "22")

  # Run
  result <- match_y(v_hap, r_hap)

  # Test
  expect_equal(length(result), 3)
  expect_equal(length(result[[1]]), 4)
  expect_equal(result[[1]][1], 0)
  expect_equal(result[[1]][2], 0)
  expect_equal(result[[1]][3], 1)
  expect_equal(result[[1]][4], 1)
  expect_equal(result[[2]][1], 0)
  expect_equal(result[[2]][2], 1)
  expect_equal(result[[2]][3], 0)
  expect_equal(result[[2]][4], 1)
  expect_equal(result[[3]][1], 0)
  expect_equal(result[[3]][2], 0)
  expect_equal(result[[3]][3], 2)
  expect_equal(result[[3]][4], 2)
})

test_that("match_y pattern 4", {

  # Condition
  v_hap <- c("10", "15, 16", "22")
  r_hap <- c("10", "15, 17", "")

  # Run
  result <- match_y(v_hap, r_hap)

  # Test
  expect_equal(length(result), 3)
  expect_equal(length(result[[1]]), 4)
  expect_equal(result[[1]][1], 0)
  expect_equal(result[[1]][2], 1)
  expect_equal(result[[1]][3], 0)
  expect_equal(result[[1]][4], 1)
  expect_equal(result[[2]][1], 0)
  expect_equal(result[[2]][2], 0)
  expect_equal(result[[2]][3], 1)
  expect_equal(result[[2]][4], 1)
  expect_equal(result[[3]][1], 0)
  expect_equal(result[[3]][2], 1)
  expect_equal(result[[3]][3], 0)
  expect_equal(result[[3]][4], 1)
})

test_that("match_y pattern 5", {

  # Condition
  v_hap <- c("10", "15, 16", "23, 24")
  r_hap <- c("10", "15, 17", "21")

  # Run
  result <- match_y(v_hap, r_hap)

  # Test
  expect_equal(length(result), 3)
  expect_equal(length(result[[1]]), 4)
  expect_equal(result[[1]][1], 0)
  expect_equal(result[[1]][2], 1)
  expect_equal(result[[1]][3], 1)
  expect_equal(result[[1]][4], 2)
  expect_equal(result[[2]][1], 0)
  expect_equal(result[[2]][2], 0)
  expect_equal(result[[2]][3], 0)
  expect_equal(result[[2]][4], 0)
  expect_equal(result[[3]][1], 0)
  expect_equal(result[[3]][2], 1)
  expect_equal(result[[3]][3], 2)
  expect_equal(result[[3]][4], 3)
})

test_that("match_y pattern 6", {

  # Condition
  v_hap <- c("10", "10, 13", "13, 14")
  r_hap <- c("10, 11, 12", "10, 11, 12", "10, 11, 12")

  # Run
  result <- match_y(v_hap, r_hap)

  # Test
  expect_equal(length(result), 3)
  expect_equal(length(result[[1]]), 4)
  expect_equal(result[[1]][1], 0)
  expect_equal(result[[1]][2], 1)
  expect_equal(result[[1]][3], 1)
  expect_equal(result[[1]][4], 2)
  expect_equal(result[[2]][1], 1)
  expect_equal(result[[2]][2], 0)
  expect_equal(result[[2]][3], 0)
  expect_equal(result[[2]][4], 1)
  expect_equal(result[[3]][1], 0)
  expect_equal(result[[3]][2], 1)
  expect_equal(result[[3]][3], 99)
  expect_equal(result[[3]][4], 100)
})
