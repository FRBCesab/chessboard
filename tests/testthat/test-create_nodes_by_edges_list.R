# Data for tests ----

edges <- data.frame("from" = c("1-1", "1-2", "2-1", "2-2", "1-1", "1-2", "1-3"),
                    "to"   = c("1-2", "1-3", "2-2", "2-3", "2-1", "2-2", "2-3"))


# Tests for errors ----

test_that("create_nodes_by_edges_list() - Tests for success", {
  
  expect_silent({ check <- create_nodes_by_edges_list(edges[0, ]) })
  
  expect_equal(class(check), "data.frame")
  expect_equal(ncol(check), 4L)
  expect_equal(nrow(check), 0L)
  expect_equal(colnames(check), c("direction", "edge", "node", "link"))
  
  expect_silent({ check <- create_nodes_by_edges_list(edges[1:4, ], 
                                                      direction = "ortho_r") })
  
  expect_equal(class(check), "data.frame")
  expect_equal(ncol(check), 4L)
  expect_equal(nrow(check), 6L)
  expect_equal(colnames(check), c("direction", "edge", "node", "link"))
  expect_equal(check[1, 2], 1L)
  expect_equal(check[2, 2], 1L)
  expect_equal(check[3, 2], 2L)
  expect_equal(check[4, 2], 3L)
  expect_equal(check[5, 2], 3L)
  expect_equal(check[6, 2], 4L)
  
  expect_silent({ check <- create_nodes_by_edges_list(edges[5:7, ], 
                                                      direction = "main") })
  
  expect_equal(class(check), "data.frame")
  expect_equal(ncol(check), 4L)
  expect_equal(nrow(check), 3L)
  expect_equal(colnames(check), c("direction", "edge", "node", "link"))
  expect_equal(check[1, 2], 1L)
  expect_equal(check[2, 2], 2L)
  expect_equal(check[3, 2], 3L)
})
