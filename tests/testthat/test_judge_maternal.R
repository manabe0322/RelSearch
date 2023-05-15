test_that("judge_maternal, simple family, character", {

  # Define a family
  id <- c("f", "m", "c1", "c2")
  fid <- c(0, 0, "f", "f")
  mid <- c(0, 0, "m", "m")
  sex <- c(1, 2, 1, 1)

  # sibling
  result_sib <- judge_maternal("c1", "c2", id, mid)
  expect_equal(result_sib, "collateral")

  # father-son
  result_fs <- judge_maternal("f", "c2", id, mid)
  expect_equal(result_fs, "not maternal")

  # mother-son
  result_ms <- judge_maternal("m", "c2", id, mid)
  expect_equal(result_ms, "lineal")

  # unrelated
  result_unr <- judge_maternal("f", "m", id, mid)
  expect_equal(result_unr, "not maternal")
})

test_that("judge_maternal, complex family, index", {

  # Define a family
  id <- 1:8
  fid <- c(0, 0, 0, 1, 1, 0, 3, 6)
  mid <- c(0, 0, 0, 2, 2, 0, 4, 5)
  sex <- c(1, 2, 1, 2, 2, 1, 1, 1)

  # grandfather-grandson
  result_17 <- judge_maternal(1, 7, id, mid)
  expect_equal(result_17, "not maternal")

  # grandmother-grandson
  result_27 <- judge_maternal(2, 7, id, mid)
  expect_equal(result_27, "lineal")

  # father-son
  result_37 <- judge_maternal(3, 7, id, mid)
  expect_equal(result_37, "not maternal")

  # mother-son
  result_47 <- judge_maternal(4, 7, id, mid)
  expect_equal(result_47, "lineal")

  # aunt-nephew
  result_57 <- judge_maternal(5, 7, id, mid)
  expect_equal(result_57, "collateral")

  # unrelated
  result_67 <- judge_maternal(6, 7, id, mid)
  expect_equal(result_67, "not maternal")

  # cousin
  result_87 <- judge_maternal(8, 7, id, mid)
  expect_equal(result_87, "collateral")
})
