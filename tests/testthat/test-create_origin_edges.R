# Data for tests ----

nodes <- c("1-1", "1-2", "1-3")


# Tests for errors ----

test_that("create_origin_edges() - Tests for success", {
  
  expect_silent({ check <- create_origin_edges(nodes) })
  
  expect_equal(class(check), "data.frame")
  expect_equal(ncol(check), 2L)
  expect_equal(nrow(check), 3L)
  expect_equal(colnames(check), c("from", "to"))
  expect_equal(check[1, 1], "0")
  expect_equal(check[2, 1], "0")
  expect_equal(check[3, 1], "0")
  expect_equal(check[1, 2], "1-1")
  expect_equal(check[2, 2], "1-2")
  expect_equal(check[3, 2], "1-3")
})
