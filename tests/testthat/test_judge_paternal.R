test_that("judge_paternal, simple family, character", {

  # Define a family
  id <- c("f", "m", "c1", "c2")
  fid <- c(0, 0, "f", "f")
  mid <- c(0, 0, "m", "m")
  sex <- c(1, 2, 1, 1)

  # sibling
  result_sib <- judge_paternal("c1", "c2", id, fid, sex)
  expect_equal(result_sib, "collateral")

  # father-son
  result_fs <- judge_paternal("f", "c2", id, fid, sex)
  expect_equal(result_fs, "lineal")

  # mother-son
  result_ms <- judge_paternal("m", "c2", id, fid, sex)
  expect_equal(result_ms, "not paternal")

  # unrelated
  result_unr <- judge_paternal("f", "m", id, fid, sex)
  expect_equal(result_unr, "not paternal")
})

test_that("judge_paternal, complex family, index", {

  # Define a family
  id <- 1:8
  fid <- c(0, 0, 0, 1, 1, 0, 4, 5)
  mid <- c(0, 0, 0, 2, 2, 0, 3, 6)
  sex <- c(1, 2, 2, 1, 1, 2, 1, 1)

  # grandfather-grandson
  result_17 <- judge_paternal(1, 7, id, fid, sex)
  expect_equal(result_17, "lineal")

  # grandmother-grandson
  result_27 <- judge_paternal(2, 7, id, fid, sex)
  expect_equal(result_27, "not paternal")

  # mother-son
  result_37 <- judge_paternal(3, 7, id, fid, sex)
  expect_equal(result_37, "not paternal")

  # father-son
  result_47 <- judge_paternal(4, 7, id, fid, sex)
  expect_equal(result_47, "lineal")

  # uncle-nephew
  result_57 <- judge_paternal(5, 7, id, fid, sex)
  expect_equal(result_57, "collateral")

  # unrelated
  result_67 <- judge_paternal(6, 7, id, fid, sex)
  expect_equal(result_67, "not paternal")

  # cousin
  result_87 <- judge_paternal(8, 7, id, fid, sex)
  expect_equal(result_87, "collateral")
})

