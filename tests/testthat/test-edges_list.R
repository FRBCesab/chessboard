# Data for tests ----

nodes <- c("S01", "S02", "S11", "S21")

nodes_num <- 1:10
nodes_fac <- as.factor(letters)
nodes_df  <- data.frame(letters, letters)
nodes_mat <- as.matrix(nodes_df)
nodes_lst <- list(letters, letters)
nodes_nul <- NULL
nodes_na  <- c(NA, letters)


# Test for errors ----

test_that("edges_list() - Tests for wrong inputs", {
  
  
  ## Test nodes argument ----
  
  expect_error(edges_list(), 
               "Argument 'nodes' is required", 
               fixed = TRUE)
  
  expect_error(edges_list(nodes_num), 
               "Argument 'nodes' must be a character vector (sites labels)", 
               fixed = TRUE)
  
  expect_error(edges_list(nodes_fac), 
               "Argument 'nodes' must be a character vector (sites labels)", 
               fixed = TRUE)

  expect_error(edges_list(nodes_df), 
               "Argument 'nodes' must be a character vector (sites labels)", 
               fixed = TRUE)
  
  expect_error(edges_list(nodes_mat), 
               "Argument 'nodes' must be a character vector (sites labels)", 
               fixed = TRUE)
  
  expect_error(edges_list(nodes_lst), 
               "Argument 'nodes' must be a character vector (sites labels)", 
               fixed = TRUE)
  
  expect_error(edges_list(nodes_nul), 
               "Argument 'nodes' must be a character vector (sites labels)", 
               fixed = TRUE)
  
  expect_error(edges_list(nodes_na), 
               "Argument 'nodes' cannot contain NA", 
               fixed = TRUE)
  
  expect_error(edges_list(rep("S21", 4)), 
               "Argument 'nodes' contain less than two nodes", 
               fixed = TRUE)
  
  
  ## Test degree argument ----
  
  expect_error(edges_list(nodes, degree = "all"), 
               "Argument 'degree' must be an integer of length 1", 
               fixed = TRUE)
  
  expect_error(edges_list(nodes, degree = 1:2), 
               "Argument 'degree' must be an integer of length 1", 
               fixed = TRUE)
  
  expect_error(edges_list(nodes, degree = length(nodes) + 1), 
               "Argument 'degree' must be 'strictly < length(nodes)'", 
               fixed = TRUE)
  
  expect_error(edges_list(nodes, degree = length(nodes)), 
               "Argument 'degree' must be 'strictly < length(nodes)'", 
               fixed = TRUE)
})


# Test for self links ----

test_that("edges_list() - Tests for self argument", {
  
  ## self is FALSE ----
  
  expect_silent({
    edges <- edges_list(nodes, self = FALSE)
  })
  
  expect_equal(ncol(edges), 4L)
  expect_equal(nrow(edges), 6L)
  expect_equal(edges[1, "edge_id"], "E-1")
  expect_equal(edges[1, "from"], "S01")
  expect_equal(edges[1, "to"], "S02")
  
  
  ## self is TRUE ----
  
  expect_silent({
    edges <- edges_list(nodes, self = TRUE)
  })
  
  expect_equal(ncol(edges), 4L)
  expect_equal(nrow(edges), 10L)
  expect_equal(edges[1, "edge_id"], "E-1")
  expect_equal(edges[1, "from"], "S01")
  expect_equal(edges[1, "to"], "S01")
})


# Test for all argument ----

test_that("edges_list() - Tests for all argument", {
  
  ## all is FALSE ----
  
  expect_silent({
    edges <- edges_list(nodes, all = FALSE)
  })
  
  expect_equal(ncol(edges), 4L)
  expect_equal(nrow(edges), 6L)
  expect_equal(edges[1, "edge_id"], "E-1")
  expect_equal(edges[1, "from"], "S01")
  expect_equal(edges[1, "to"], "S02")
  
  
  ## all is TRUE ----
  
  expect_silent({
    edges <- edges_list(nodes, all = TRUE)
  })
  
  expect_equal(ncol(edges), 4L)
  expect_equal(nrow(edges), length(nodes) ^ 2)
  expect_equal(edges[1, "edge_id"], "")
  expect_equal(edges[1, "edge"], 0)
  expect_equal(edges[1, "from"], "S01")
  expect_equal(edges[1, "to"], "S01")
  expect_equal(sum(edges$"edge"), 6L)
})


# Test for directed argument ----

test_that("edges_list() - Tests for directed argument", {
  
  ## directed is FALSE ----
  
  expect_silent({
    edges <- edges_list(nodes, directed = FALSE)
  })
  
  expect_equal(ncol(edges), 4L)
  expect_equal(nrow(edges), 6L)
  expect_equal(edges[1, "edge_id"], "E-1")
  expect_equal(edges[1, "from"], "S01")
  expect_equal(edges[1, "to"], "S02")
  
  
  ## directed is TRUE ----
  
  expect_silent({
    edges <- edges_list(nodes, directed = TRUE)
  })
  
  expect_equal(ncol(edges), 4L)
  expect_equal(nrow(edges), 3L)
  expect_equal(edges[1, "edge_id"], "E-1")
  expect_equal(edges[2, "edge_id"], "E-2")
  expect_equal(edges[1, "from"], "S01")
  expect_equal(edges[1, "to"], "S02")
})
