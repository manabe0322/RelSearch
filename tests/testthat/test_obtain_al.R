test_that("obtain_al pattern 1", {
  al <- obtain_al("15")
  expect_equal(length(al), 1)
  expect_equal(al[1], 15)
})

test_that("obtain_al pattern 2-1", {
  al <- obtain_al("15,16")
  expect_equal(length(al), 2)
  expect_equal(al[1], 15)
  expect_equal(al[2], 16)
})

test_that("obtain_al pattern 2-2", {
  al <- obtain_al("15, 16")
  expect_equal(length(al), 2)
  expect_equal(al[1], 15)
  expect_equal(al[2], 16)
})

test_that("obtain_al pattern 3-1", {
  al <- obtain_al("15,16,17.1")
  expect_equal(length(al), 3)
  expect_equal(al[1], 15)
  expect_equal(al[2], 16)
  expect_equal(al[3], 17.1)
})

test_that("obtain_al pattern 3-2", {
  al <- obtain_al("15, 16, 17.1")
  expect_equal(length(al), 3)
  expect_equal(al[1], 15)
  expect_equal(al[2], 16)
  expect_equal(al[3], 17.1)
})

test_that("obtain_al pattern 4-1", {
  al <- obtain_al("")
  expect_equal(al, numeric(0))
})

test_that("obtain_al pattern 4-2", {
  al <- obtain_al(" ")
  expect_equal(al, numeric(0))
})
