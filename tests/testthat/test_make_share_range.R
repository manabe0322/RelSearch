test_that("make_share_range pattern 1", {
  pos_mt_vr <- c(73:340, 16024:16365)
  share_range <- make_share_range(pos_mt_vr)
  expect_equal(share_range, "73-340 16024-16365")
})

test_that("make_share_range pattern 2", {
  pos_mt_vr <- c(340, 16024:16365)
  share_range <- make_share_range(pos_mt_vr)
  expect_equal(share_range, "340 16024-16365")
})

test_that("make_share_range pattern 3", {
  range_victim <- "72-339 16024-16365"
  range_reference <- "73-340 16025-16364"
  pos_mt_vr <- extract_pos_mt_vr(range_victim, range_reference)
  share_range <- make_share_range(pos_mt_vr)
  expect_equal(share_range, "73-339 16025-16364")
})
