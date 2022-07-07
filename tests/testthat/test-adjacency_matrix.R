# Data for tests ----

nodes <- c("S01", "S02", "S11", "S21")
edges <- edges_list(nodes)
edges_all <- edges_list(nodes, all = TRUE, self = TRUE)

edges_edgeid <- edges[ , -1, drop = FALSE]
edges_edge   <- edges[ , -2, drop = FALSE]
edges_from   <- edges[ , -3, drop = FALSE]
edges_to     <- edges[ , -4, drop = FALSE]
edges_empty  <- edges[0, ]


# Test for errors ----

test_that("adjacency_matrix() - Tests for wrong inputs", {
  
  ## Test edges argument ----
  
  expect_error(adjacency_matrix(), 
               "Argument 'edges' is required", 
               fixed = TRUE)
  
  expect_error(adjacency_matrix(nodes), 
               "Argument 'edges' must be a data.frame", 
               fixed = TRUE)
  
  expect_error(adjacency_matrix(edges_edgeid), 
               "The column 'edge_id' is absent from the edges data.frame", 
               fixed = TRUE)
  
  expect_error(adjacency_matrix(edges_edge), 
               "The column 'edge' is absent from the edges data.frame", 
               fixed = TRUE)
  
  expect_error(adjacency_matrix(edges_from), 
               "The column 'from' is absent from the edges data.frame", 
               fixed = TRUE)
  
  expect_error(adjacency_matrix(edges_to), 
               "The column 'to' is absent from the edges data.frame", 
               fixed = TRUE)
  
  expect_error(adjacency_matrix(edges_empty), 
               "Argument 'edges' must have at least one row", 
               fixed = TRUE)
})


# Test for success ----

test_that("adjacency_matrix() - Tests for good outputs", {
  
  ## Default settings (not all edges) ----
  
  expect_silent({
    adja_mat <- adjacency_matrix(edges)
  })
  
  expect_equal(class(adja_mat), c("matrix", "array"))
  expect_equal(dim(adja_mat), c(length(nodes), length(nodes)))
  expect_true(is.numeric(adja_mat))
  expect_true(sum(adja_mat) == sum(edges$"edge"))
  expect_equal(adja_mat[2, 1], adja_mat[1, 2])
  expect_equal(adja_mat[2, 2], 0L)
  
  
  ## Default settings (all edges) ----
  
  expect_silent({
    adja_mat <- adjacency_matrix(edges_all)
  })
  
  expect_equal(dim(adja_mat), c(length(nodes), length(nodes)))
  expect_true(sum(adja_mat) == sum(edges_all$"edge"))
  expect_equal(adja_mat[2, 1], adja_mat[1, 2])
  expect_equal(adja_mat[2, 2], 1L)
  
  
  ## Upper is FALSE ----
  
  expect_silent({
    adja_mat <- adjacency_matrix(edges, upper = FALSE)
  })
  
  expect_equal(adja_mat[2, 1], 1L)
  expect_true(is.na(adja_mat[1, 2]))
  
  
  ## Lower is FALSE ----
  
  expect_silent({
    adja_mat <- adjacency_matrix(edges, lower = FALSE)
  })
  
  expect_equal(adja_mat[1, 2], 1L)
  expect_true(is.na(adja_mat[2, 1]))
  
  
  ## Upper and Lower are FALSE ----
  
  expect_silent({
    adja_mat <- adjacency_matrix(edges, upper = FALSE, lower = FALSE)
  })
  
  expect_true(is.na(adja_mat[1, 2])) 
  expect_true(is.na(adja_mat[2, 1])) 
  
  
  ## Diag is FALSE ----
  
  expect_silent({
    adja_mat <- adjacency_matrix(edges, diag = FALSE)
  })
  
  expect_true(is.na(adja_mat[1, 1])) 
  expect_true(is.na(adja_mat[3, 3])) 
  
  
  ## Upper, Lower, and Diag are FALSE ----
  
  expect_silent({
    adja_mat <- adjacency_matrix(edges, diag = FALSE, upper = FALSE, 
                                 lower = FALSE)
  })
  
  expect_true(sum(is.na(adja_mat)) == length(adja_mat)) 
  
  
  ## na_to_zero is FALSE ----
  
  expect_silent({
    adja_mat <- adjacency_matrix(edges, na_to_zero = FALSE)
  })
  
  expect_true(is.na(adja_mat[1, 1])) 
  expect_true(is.na(adja_mat[1, 3])) 
  expect_true(is.na(adja_mat[3, 1])) 
})