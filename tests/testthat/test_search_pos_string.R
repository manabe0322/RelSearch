test_that("search_pos_string", {

  # Condition
  names_rel <- c("parent-child", "sibling", "grandparent-grandchild", "uncle-nephew", "cousin")

  # Test
  expect_equal(search_pos_string(names_rel, "parent-child"), 0)
  expect_equal(search_pos_string(names_rel, "sibling"), 1)
  expect_equal(search_pos_string(names_rel, "grandparent-grandchild"), 2)
  expect_equal(search_pos_string(names_rel, "uncle-nephew"), 3)
  expect_equal(search_pos_string(names_rel, "cousin"), 4)
  expect_equal(search_pos_string(names_rel, "twins"), 5)
})
