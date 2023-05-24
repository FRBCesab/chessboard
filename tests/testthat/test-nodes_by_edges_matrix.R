# Data for tests ----

edges <- data.frame("from" = c("1-1", "2-1", "3-1",
                               "1-1", "2-1",
                               "1-2", "2-2",
                               "3-1", "2-1",
                               "3-2", "2-2"), 
                    "to"   = c("1-2", "2-2", "3-2", 
                               "2-1", "3-1",
                               "2-2", "3-2",
                               "2-1", "1-1",
                               "2-2", "1-2"))

edges_bad <- data.frame("from" = c("1-1", "1-2", "1-3",
                                   "2-1", "2-2", "2-3",
                                   "1-1", "1-2", "2-1", "2-2", 
                                   "1-3", "1-2", "2-3", "2-2"),
                        "to"   = c("2-1", "2-2", "2-3",
                                   "1-1", "1-2", "1-3",
                                   "1-2", "1-3", "2-2", "2-3", 
                                   "1-2", "1-1", "2-2", "2-1"))


# Tests for errors ----

test_that("nodes_by_edges_matrix() - Tests for errors", {
  
  expect_error(nodes_by_edges_matrix(edges_bad),
               "This function is not designed to deal with undirected network",
               fixed = TRUE)
  
  expect_error(nodes_by_edges_matrix(edges[-c(1:3), ]),
               paste0("The network does not seem to have a main direction ", 
                      "(no edges along transects have been detected)"),
               fixed = TRUE)
})
  


# Tests for success ----

test_that("nodes_by_edges_matrix() - Tests for success", {
  
  expect_silent({ check <- nodes_by_edges_matrix(edges[1:3, ]) })
  
  expect_equal(class(check), "list")
  expect_equal(names(check), c("se.mat", "edges"))
  
  expect_equal(nrow(check$"se.mat"), 6L)
  expect_equal(ncol(check$"se.mat"), 6L)
  expect_equal(sum(check$"se.mat"), 9L)
  
  expect_equal(nrow(check$"edges"), 6L)
  expect_equal(ncol(check$"edges"), 2L)
  
  expect_equal(check$"edges"[1, 1], "0")
  expect_equal(check$"edges"[2, 1], "0")
  expect_equal(check$"edges"[3, 1], "0")
  
  expect_equal(check$"edges"[1, 2], "1-1")
  expect_equal(check$"edges"[2, 2], "2-1")
  expect_equal(check$"edges"[3, 2], "3-1")
  
  
  expect_silent({ check <- nodes_by_edges_matrix(edges[1:7, ]) })
  
  expect_equal(class(check), "list")
  expect_equal(names(check), c("se.mat", "edges"))
  
  expect_equal(nrow(check$"se.mat"), 6L)
  expect_equal(ncol(check$"se.mat"), 12L)
  expect_equal(sum(check$"se.mat"), 21L)
  
  expect_equal(nrow(check$"edges"), 12L)
  expect_equal(ncol(check$"edges"), 2L)
  
  expect_equal(check$"edges"[1, 1], "0")
  expect_equal(check$"edges"[2, 1], "0")
  expect_equal(check$"edges"[3, 1], "0")
  expect_equal(check$"edges"[7, 1], "0")
  expect_equal(check$"edges"[8, 1], "0")
  
  expect_equal(check$"edges"[1, 2], "1-1")
  expect_equal(check$"edges"[2, 2], "2-1")
  expect_equal(check$"edges"[3, 2], "3-1")
  expect_equal(check$"edges"[7, 2], "1-1")
  expect_equal(check$"edges"[8, 2], "1-2")
  
  
  expect_silent({ check <- nodes_by_edges_matrix(edges[c(1:3, 8:11), ]) })
  
  expect_equal(class(check), "list")
  expect_equal(names(check), c("se.mat", "edges"))
  
  expect_equal(nrow(check$"se.mat"), 6L)
  expect_equal(ncol(check$"se.mat"), 12L)
  expect_equal(sum(check$"se.mat"), 21L)
  
  expect_equal(nrow(check$"edges"), 12L)
  expect_equal(ncol(check$"edges"), 2L)
  
  expect_equal(check$"edges"[1, 1], "0")
  expect_equal(check$"edges"[2, 1], "0")
  expect_equal(check$"edges"[3, 1], "0")
  expect_equal(check$"edges"[7, 1], "0")
  expect_equal(check$"edges"[8, 1], "0")
  
  expect_equal(check$"edges"[1, 2], "1-1")
  expect_equal(check$"edges"[2, 2], "2-1")
  expect_equal(check$"edges"[3, 2], "3-1")
  expect_equal(check$"edges"[7, 2], "3-1")
  expect_equal(check$"edges"[8, 2], "3-2")
  
  
  expect_silent({ check <- nodes_by_edges_matrix(edges) })
  
  expect_equal(class(check), "list")
  expect_equal(names(check), c("se.mat", "edges"))
  
  expect_equal(nrow(check$"se.mat"), 6L)
  expect_equal(ncol(check$"se.mat"), 18L)
  expect_equal(sum(check$"se.mat"), 33L)
  
  expect_equal(nrow(check$"edges"), 18L)
  expect_equal(ncol(check$"edges"), 2L)
  
  expect_equal(check$"edges"[1, 1], "0")
  expect_equal(check$"edges"[2, 1], "0")
  expect_equal(check$"edges"[3, 1], "0")
  expect_equal(check$"edges"[7, 1], "0")
  expect_equal(check$"edges"[8, 1], "0")
  expect_equal(check$"edges"[13, 1], "0")
  expect_equal(check$"edges"[14, 1], "0")
  
  expect_equal(check$"edges"[1, 2], "1-1")
  expect_equal(check$"edges"[2, 2], "2-1")
  expect_equal(check$"edges"[3, 2], "3-1")
  expect_equal(check$"edges"[7, 2], "1-1")
  expect_equal(check$"edges"[8, 2], "1-2")
  expect_equal(check$"edges"[13, 2], "3-1")
  expect_equal(check$"edges"[14, 2], "3-2")
})
