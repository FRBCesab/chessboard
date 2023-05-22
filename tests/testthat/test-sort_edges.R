# Data for tests ----

edges <- data.frame("from" = c("1-1", "1-2"), 
                    "to"   = c("1-2", "1-3"), 
                    "edge" = 1)


# Tests for errors ----

test_that("sort_edges() - Tests for success", {
  
  expect_silent({ check <- sort_edges(edges[2:1, ]) })
  
  expect_equal(class(check), "data.frame")
  expect_equal(ncol(check), 2L)
  expect_equal(nrow(check), 2L)
  expect_equal(colnames(check), c("from", "to"))
  expect_equal(check[1, 1], "1-1")
  expect_equal(check[2, 1], "1-2")
  expect_equal(check[1, 2], "1-2")
  expect_equal(check[2, 2], "1-3")
})
