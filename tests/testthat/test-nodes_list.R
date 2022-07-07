# Data for tests ----

nodes <- c("S01", "S02", "S11", "S21")
edges <- edges_list(nodes)[ , -c(1:2)]

edges_from <- edges[ , -1, drop = FALSE]
edges_to   <- edges[ , -2, drop = FALSE]


# Test for errors ----

test_that("nodes_list() - Tests for wrong inputs", {
  
  expect_error(nodes_list(), 
               "Argument 'x' is required", 
               fixed = TRUE)
  
  expect_error(nodes_list(nodes), 
               "Argument 'x' must be a data.frame", 
               fixed = TRUE)
  
  expect_error(nodes_list(edges_from), 
               "The column 'from' is absent from the edges data.frame", 
               fixed = TRUE)
  
  expect_error(nodes_list(edges_to), 
               "The column 'to' is absent from the edges data.frame", 
               fixed = TRUE)
})


# Test for success ----

test_that("nodes_list() - Tests for good outputs", {
  
  expect_silent({
    nodes <- nodes_list(edges)
  })
  
  expect_equal(class(nodes), "data.frame")
  expect_equal(ncol(nodes), 1L)
  expect_equal(nrow(nodes), 4L)
  expect_equal(colnames(nodes), "node")
  expect_equal(nodes[1, 1], "S01")
  expect_equal(nodes[3, 1], "S11")
})
