# Data for tests ----

mat_bad_0 <- matrix(data = rep(c(0, 1, 0), 3), 
                    nrow = 3, 
                    ncol = 3)

mat_bad_1 <- matrix(data = rep(c(0, 1, 0), 3), 
                    nrow = 3, 
                    ncol = 3)

colnames(mat_bad_1) <- c("1-1", "1-2", "1-3")
rownames(mat_bad_1) <- c("1-1", "1-2", "X-X")

mat_bad_2 <- matrix(data = rep(0, 9), 
                    nrow = 3, 
                    ncol = 3)

colnames(mat_bad_2) <- c("1-1", "1-2", "1-3")
rownames(mat_bad_2) <- c("1-1", "1-2", "1-3")

mat <- matrix(data = rep(c(0, 1, 0), 3), 
              nrow = 3, 
              ncol = 3)

colnames(mat) <- c("1-1", "1-2", "1-3")
rownames(mat) <- c("1-1", "1-2", "1-3")


# Tests for errors ----

test_that("matrix_to_edges_list() - Tests for errors", {
  
  expect_error(matrix_to_edges_list(),
               "Argument 'x' is required",
               fixed = TRUE)
  
  expect_error(matrix_to_edges_list(1),
               "Argument 'x' must be a matrix (connectivity matrix)",
               fixed = TRUE)
  
  expect_error(matrix_to_edges_list(letters),
               "Argument 'x' must be a matrix (connectivity matrix)",
               fixed = TRUE)
  
  expect_error(matrix_to_edges_list(data.frame(1, 2)),
               "Argument 'x' must be a matrix (connectivity matrix)",
               fixed = TRUE)
  
  expect_error(matrix_to_edges_list(matrix(letters, ncol = 2)),
               "Argument 'x' must be a numeric matrix (connectivity matrix)",
               fixed = TRUE)
  
  expect_error(matrix_to_edges_list(matrix(c(TRUE, FALSE, TRUE, FALSE), 
                                           ncol = 2)),
               "Argument 'x' must be a numeric matrix (connectivity matrix)",
               fixed = TRUE)
  
  expect_error(matrix_to_edges_list(mat[-1, ]),
               paste0("Number of rows of 'x' must be equal to number of ", 
                      "columns (connectivity matrix)"),
               fixed = TRUE)
  
  expect_error(matrix_to_edges_list(mat[ , -1]),
               paste0("Number of rows of 'x' must be equal to number of ", 
                      "columns (connectivity matrix)"),
               fixed = TRUE)
  
  expect_error(matrix_to_edges_list(mat_bad_0),
               "Row names of 'x' must contain nodes labels",
               fixed = TRUE)
  
  expect_error(matrix_to_edges_list(mat_bad_1),
               "Row names and column names of 'x' must be equal",
               fixed = TRUE)
  
  expect_error(matrix_to_edges_list(mat_bad_2),
               "Argument 'x' contains no edge",
               fixed = TRUE)
})


# Tests for errors ----

test_that("matrix_to_edges_list() - Tests for success", {
  
  expect_silent({ check <- matrix_to_edges_list(mat) })
  
  expect_true("data.frame" %in% class(check))
  expect_equal(ncol(check), 3L)
  expect_equal(nrow(check), sum(mat))
  
  expect_true(colnames(check)[1] == "from")
  expect_true(colnames(check)[2] == "to")
  expect_true(colnames(check)[3] == "edge")
  
  expect_equal(check[1, 1], "1-2")
  expect_equal(check[2, 1], "1-2")
  expect_equal(check[3, 1], "1-2")
  
  expect_equal(check[1, 2], "1-1")
  expect_equal(check[2, 2], "1-2")
  expect_equal(check[3, 2], "1-3")
  
  expect_equal(check[1, 3], 1L)
  expect_equal(check[2, 3], 1L)
  expect_equal(check[3, 3], 1L)
  
  
  expect_silent({ check <- matrix_to_edges_list(mat, all = TRUE) })
  
  expect_true("data.frame" %in% class(check))
  expect_equal(ncol(check), 3L)
  expect_equal(nrow(check), nrow(mat) * ncol(mat))
  
  expect_true(colnames(check)[1] == "from")
  expect_true(colnames(check)[2] == "to")
  expect_true(colnames(check)[3] == "edge")
  
  expect_equal(check[1, 1], "1-1")
  expect_equal(check[2, 1], "1-1")
  expect_equal(check[3, 1], "1-1")
  expect_equal(check[4, 1], "1-2")
  expect_equal(check[5, 1], "1-2")
  expect_equal(check[6, 1], "1-2")
  expect_equal(check[7, 1], "1-3")
  expect_equal(check[8, 1], "1-3")
  expect_equal(check[9, 1], "1-3")
  
  expect_equal(check[1, 2], "1-1")
  expect_equal(check[2, 2], "1-2")
  expect_equal(check[3, 2], "1-3")
  expect_equal(check[4, 2], "1-1")
  expect_equal(check[5, 2], "1-2")
  expect_equal(check[6, 2], "1-3")
  expect_equal(check[7, 2], "1-1")
  expect_equal(check[8, 2], "1-2")
  expect_equal(check[9, 2], "1-3")
  
  expect_equal(check[1, 3], 0L)
  expect_equal(check[2, 3], 0L)
  expect_equal(check[3, 3], 0L)
  expect_equal(check[4, 3], 1L)
  expect_equal(check[5, 3], 1L)
  expect_equal(check[6, 3], 1L)
  expect_equal(check[7, 3], 0L)
  expect_equal(check[8, 3], 0L)
  expect_equal(check[9, 3], 0L)
  
})
