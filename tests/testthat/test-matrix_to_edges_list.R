# Data for tests ----

nodes <- c("S01", "S02", "S11", "S21")

edges     <- edges_list(nodes)
edges_all <- edges_list(nodes, all = TRUE)
adja_mat  <- adjacency_matrix(edges)

adja_bad1 <- adja_mat
rownames(adja_bad1) <- NULL

adja_bad2 <- adja_mat
rownames(adja_bad2)[1] <- "XXX"

adja_bad3 <- adja_mat
colnames(adja_bad3)[1] <- "XXX"

adja_bad4 <- adja_mat
adja_bad4[1:4, 1:4] <- NA
adja_bad4[1, 1] <- 0


# Test for errors ----

test_that("matrix_to_edges_list() - Tests for wrong inputs", {
  
  ## Test x argument ----
  
  expect_error(matrix_to_edges_list(), 
               "Argument 'x' is required", 
               fixed = TRUE)
  
  expect_error(matrix_to_edges_list(rep(1, 10)), 
               "Argument 'x' must be a matrix (adjacency matrix)", 
               fixed = TRUE)
  
  expect_error(matrix_to_edges_list(nodes), 
               "Argument 'x' must be a matrix (adjacency matrix)", 
               fixed = TRUE)
  
  expect_error(matrix_to_edges_list(edges), 
               "Argument 'x' must be a matrix (adjacency matrix)", 
               fixed = TRUE)
  
  expect_error(matrix_to_edges_list(matrix(letters, ncol = 2)), 
               "Argument 'x' must be a numeric matrix (adjacency matrix)", 
               fixed = TRUE)
  
  expect_error(matrix_to_edges_list(adja_mat[-1, ]), 
               paste0("Number of rows of 'x' must be equal to number of ", 
                      "columns (adjacency matrix)"), 
               fixed = TRUE)
  
  expect_error(matrix_to_edges_list(adja_mat[ , -1]), 
               paste0("Number of rows of 'x' must be equal to number of ", 
                      "columns (adjacency matrix)"), 
               fixed = TRUE)
  
  expect_error(matrix_to_edges_list(adja_bad1), 
               "Row names of 'x' must contain nodes labels", 
               fixed = TRUE)
  
  expect_error(matrix_to_edges_list(adja_bad2), 
               "Row names and column names of 'x' must be equal", 
               fixed = TRUE)
  
  expect_error(matrix_to_edges_list(adja_bad3), 
               "Row names and column names of 'x' must be equal", 
               fixed = TRUE)
  
  expect_error(matrix_to_edges_list(adja_bad4), 
               "Argument 'x' contains no edge", 
               fixed = TRUE)
})


# Test for success ----

test_that("matrix_to_edges_list() - Tests for good outputs", {
  
  ## Test all is FALSE ----
  
  expect_silent({
    new_edges <- matrix_to_edges_list(adja_mat)
  })
  
  expect_true(identical(edges, new_edges))
  
  
  ## Test all is TRUE ----
  
  expect_silent({
    new_edges <- matrix_to_edges_list(adja_mat, all = TRUE)
  })
  
  expect_true(identical(edges_all, new_edges))
})
