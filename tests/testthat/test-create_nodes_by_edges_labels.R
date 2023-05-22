# Data for tests ----

edges <- data.frame("from" = c("1-1", "1-2", "2-1", "2-2", "1-1", "1-2", "1-3"), 
                    "to"   = c("1-2", "1-3", "2-2", "2-3", "2-1", "2-2", "2-3"))

nodes_edges <- rbind(
  create_nodes_by_edges_list(edges[1:4, ], direction = "main"),
  create_nodes_by_edges_list(edges[5:7, ], direction = "ortho_r"))


# Tests for success ----

test_that("create_nodes_by_edges_labels() - Tests for success", {
  
  expect_silent({ check <- create_nodes_by_edges_labels(nodes_edges) })
  
  expect_equal(class(check), "data.frame")
  expect_equal(ncol(check), 3L)
  expect_equal(nrow(check), nrow(nodes_edges))
  expect_equal(colnames(check), c("edge_id", "node", "link"))
  
  expect_equal(check[1, 1], "E-1")
  expect_equal(check[2, 1], "E-1")
  expect_equal(check[3, 1], "E-2")
  expect_equal(check[4, 1], "E-3")
  expect_equal(check[5, 1], "E-3")
  expect_equal(check[6, 1], "E-4")
  expect_equal(check[7, 1], "E-5")
  expect_equal(check[8, 1], "E-6")
  expect_equal(check[9, 1], "E-7")
})
