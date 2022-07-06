# Data for tests ----

nodes <- c("S01", "S02", "S11", "S21")

edges      <- edges_list(nodes)[ , -1]
edges_all  <- edges_list(nodes, all = TRUE)[ , -1]

edges_edge <- edges[ , -1]
edges_from <- edges[ , -2]
edges_to   <- edges[ , -3]


# Test for errors ----

test_that("create_edges_id() - Tests for wrong inputs", {
  
  expect_error(create_edges_id(), 
               "Argument 'edges' is required", 
               fixed = TRUE)
  
  expect_error(create_edges_id(nodes), 
               "Argument 'edges' must be a data.frame", 
               fixed = TRUE)
  
  expect_error(create_edges_id(edges_edge), 
               "The column 'edge' is absent from the edges data.frame", 
               fixed = TRUE)
  
  expect_error(create_edges_id(edges_from), 
               "The column 'from' is absent from the edges data.frame", 
               fixed = TRUE)
  
  expect_error(create_edges_id(edges_to), 
               "The column 'to' is absent from the edges data.frame", 
               fixed = TRUE)
})


# Test for success ----

test_that("create_edges_id() - Tests for good outputs", {
  
  expect_silent({
    edges_id <- create_edges_id(edges)
  })

  
  expect_equal(colnames(edges_id)[1], "edge_id")
  expect_equal(edges_id[1, 1], "E-1")
  expect_equal(edges_id[1, 2], 1)
  expect_equal(edges_id[1, 3], "S01")
  expect_equal(edges_id[1, 4], "S02")

  
  expect_silent({
    edges_id <- create_edges_id(edges_all)
  })

  expect_equal(edges_id[1, 1], "")
  expect_equal(edges_id[1, 2], 0)
  expect_equal(edges_id[1, 3], "S01")
  expect_equal(edges_id[1, 4], "S01")
})
