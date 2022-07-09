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

adja_bad5 <- adja_mat
rownames(adja_bad5) <- gsub("S", "X", colnames(adja_bad5))
colnames(adja_bad5) <- rownames(adja_bad5)

# Test for errors ----

test_that("spatial_weights_matrix() - Tests for wrong inputs", {
  
  ## Test x argument ----
  
  expect_error(spatial_weights_matrix(), 
               "Argument 'x' is required", 
               fixed = TRUE)
  
  expect_error(spatial_weights_matrix(rep(1, 10)), 
               "Argument 'x' must be a matrix (adjacency matrix)", 
               fixed = TRUE)
  
  expect_error(spatial_weights_matrix(nodes), 
               "Argument 'x' must be a matrix (adjacency matrix)", 
               fixed = TRUE)
  
  expect_error(spatial_weights_matrix(edges), 
               "Argument 'x' must be a matrix (adjacency matrix)", 
               fixed = TRUE)
  
  expect_error(spatial_weights_matrix(matrix(letters, ncol = 2)), 
               "Argument 'x' must be a numeric matrix (adjacency matrix)", 
               fixed = TRUE)
  
  expect_error(spatial_weights_matrix(adja_mat[-1, ]), 
               paste0("Number of rows of 'x' must be equal to number of ", 
                      "columns (adjacency matrix)"), 
               fixed = TRUE)
  
  expect_error(spatial_weights_matrix(adja_mat[ , -1]), 
               paste0("Number of rows of 'x' must be equal to number of ", 
                      "columns (adjacency matrix)"), 
               fixed = TRUE)
  
  expect_error(spatial_weights_matrix(adja_bad1), 
               "Row names of 'x' must contain nodes labels", 
               fixed = TRUE)
  
  expect_error(spatial_weights_matrix(adja_bad2), 
               "Row names and column names of 'x' must be equal", 
               fixed = TRUE)
  
  expect_error(spatial_weights_matrix(adja_bad3), 
               "Row names and column names of 'x' must be equal", 
               fixed = TRUE)
  
  expect_error(spatial_weights_matrix(adja_bad4), 
               "Argument 'x' contains no edge", 
               fixed = TRUE)
  
  
  ## Test y argument ----
  
  expect_error(spatial_weights_matrix(adja_mat), 
               "Argument 'y' is required", 
               fixed = TRUE)
  
  expect_error(spatial_weights_matrix(adja_mat, rep(1, 10)), 
               "Argument 'y' must be a matrix (edges weight matrix)", 
               fixed = TRUE)
  
  expect_error(spatial_weights_matrix(adja_mat, nodes), 
               "Argument 'y' must be a matrix (edges weight matrix)", 
               fixed = TRUE)
  
  expect_error(spatial_weights_matrix(adja_mat, edges), 
               "Argument 'y' must be a matrix (edges weight matrix)", 
               fixed = TRUE)
  
  expect_error(spatial_weights_matrix(adja_mat, matrix(letters, ncol = 2)), 
               "Argument 'y' must be a numeric matrix (edges weight matrix)", 
               fixed = TRUE)
  
  expect_error(spatial_weights_matrix(adja_mat, adja_mat[-1, ]), 
               paste0("Number of rows of 'y' must be equal to number of ", 
                      "columns (edges weight matrix)"), 
               fixed = TRUE)
  
  expect_error(spatial_weights_matrix(adja_mat, adja_mat[ , -1]), 
               paste0("Number of rows of 'y' must be equal to number of ", 
                      "columns (edges weight matrix)"), 
               fixed = TRUE)
  
  expect_error(spatial_weights_matrix(adja_mat, adja_bad1), 
               "Row names of 'y' must contain nodes labels", 
               fixed = TRUE)
  
  expect_error(spatial_weights_matrix(adja_mat, adja_bad2), 
               "Row names and column names of 'y' must be equal", 
               fixed = TRUE)
  
  expect_error(spatial_weights_matrix(adja_mat, adja_bad3), 
               "Row names and column names of 'y' must be equal", 
               fixed = TRUE)
  
  expect_error(spatial_weights_matrix(adja_mat, adja_bad4), 
               "Argument 'y' contains no weight", 
               fixed = TRUE)
  
  
  ## Test both x and y arguments ----
  
  expect_error(spatial_weights_matrix(adja_mat, adja_mat[-1, -1]), 
               paste0("Number of nodes is not equal between 'x' ", 
                      "(adjacency matrix) and 'y' (edges weight matrix)"), 
               fixed = TRUE)
  
  expect_error(spatial_weights_matrix(adja_mat[-1, -1], adja_mat), 
               paste0("Number of nodes is not equal between 'x' ", 
                      "(adjacency matrix) and 'y' (edges weight matrix)"), 
               fixed = TRUE)
  
  expect_error(spatial_weights_matrix(adja_mat, adja_bad5), 
               paste0("Nodes names do not match between 'x' ", 
                      "(adjacency matrix) and 'y' (edges weight matrix)"), 
               fixed = TRUE)
  
  expect_error(spatial_weights_matrix(adja_bad5, adja_mat), 
               paste0("Nodes names do not match between 'x' ", 
                      "(adjacency matrix) and 'y' (edges weight matrix)"), 
               fixed = TRUE)
  
  expect_error(spatial_weights_matrix(adja_mat[4:1, ], adja_mat), 
               paste0("Nodes names are not in the same order in 'x' ", 
                      "(adjacency matrix) and 'y' (edges weight matrix)"), 
               fixed = TRUE)
  
  expect_error(spatial_weights_matrix(adja_mat, adja_mat[4:1, ]), 
               paste0("Nodes names are not in the same order in 'x' ", 
                      "(adjacency matrix) and 'y' (edges weight matrix)"), 
               fixed = TRUE)
  
  expect_error(spatial_weights_matrix(adja_mat[ , 4:1 ], adja_mat), 
               paste0("Nodes names are not in the same order in 'x' ", 
                      "(adjacency matrix) and 'y' (edges weight matrix)"), 
               fixed = TRUE)
  
  expect_error(spatial_weights_matrix(adja_mat, adja_mat[ , 4:1]), 
               paste0("Nodes names are not in the same order in 'x' ", 
                      "(adjacency matrix) and 'y' (edges weight matrix)"), 
               fixed = TRUE)
})


# Test for success ----

test_that("spatial_weights_matrix() - Tests for good outputs", {
  
  ## Test all is FALSE ----
  
  expect_silent({
    spatial_weights <- spatial_weights_matrix(adja_mat, adja_mat)
  })
  
  expect_equal(class(spatial_weights), c("matrix", "array"))
  expect_equal(dim(spatial_weights), c(nrow(adja_mat), ncol(adja_mat)))
  expect_true(is.numeric(spatial_weights))
  expect_equal(sum(is.na(spatial_weights)), 0L)
  expect_equal(spatial_weights[1, 1], 4L)
  expect_equal(spatial_weights[2, 1], 1L)
  expect_equal(spatial_weights[1, 2], 1L)
})
