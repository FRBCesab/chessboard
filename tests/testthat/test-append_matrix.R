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

mat_1 <- matrix(data = rep(c(0, 1, 0), 3), 
                nrow = 3, 
                ncol = 3)

colnames(mat_1) <- c("1-1", "1-2", "1-3")
rownames(mat_1) <- c("1-1", "1-2", "1-3")

mat_2 <- matrix(data = rep(c(0, 1, 0), 3), 
                nrow = 3, 
                ncol = 3)

colnames(mat_2) <- c("2-1", "2-2", "2-3")
rownames(mat_2) <- c("2-1", "2-2", "2-3")

mat_3 <- matrix(data = rep(c(1, 0, 1, 1), 4), 
                nrow = 4, 
                ncol = 4)

colnames(mat_3) <- c("2-1", "2-2", "2-3", "1-4")
rownames(mat_3) <- c("2-1", "2-2", "2-3", "1-4")

mat_4 <- matrix(data = rep(c(1, NA, 1, 1), 4), 
                nrow = 4, 
                ncol = 4)

colnames(mat_4) <- c("2-1", "2-2", "2-3", "1-4")
rownames(mat_4) <- c("2-1", "2-2", "2-3", "1-4")


# Tests for errors ----

test_that("append_matrix() - Tests for errors", {
  
  expect_error(append_matrix(),
               "Please provide at least one matrix",
               fixed = TRUE)
  
  expect_error(append_matrix(1),
               "This function only works with 'matrix' objects",
               fixed = TRUE)
  
  expect_error(append_matrix(letters),
               "This function only works with 'matrix' objects",
               fixed = TRUE)
  
  expect_error(append_matrix(data.frame(1, 2)),
               "This function only works with 'matrix' objects",
               fixed = TRUE)
  
  expect_error(append_matrix(matrix(letters, ncol = 2)),
               "This function only works with numeric matrices",
               fixed = TRUE)
  
  expect_error(append_matrix(matrix(c(TRUE, FALSE, TRUE, FALSE), ncol = 2)),
               "This function only works with numeric matrices",
               fixed = TRUE)
  
  expect_error(append_matrix(mat_1[-1, ]),
               paste0("Number of rows of matrices must be equal to number of ", 
                      "columns (connectivity matrix)"),
               fixed = TRUE)
  
  expect_error(append_matrix(mat_1[ , -1]),
               paste0("Number of rows of matrices must be equal to number of ", 
                      "columns (connectivity matrix)"),
               fixed = TRUE)
  
  expect_error(append_matrix(mat_bad_0),
               "Row names of matrices must contain node labels",
               fixed = TRUE)
  
  expect_error(append_matrix(mat_bad_1),
               "Row names and column names of matrices must be equal",
               fixed = TRUE)
  
  expect_error(append_matrix(mat_bad_2),
               "Some matrices do not contain any edges",
               fixed = TRUE)
  
  
  expect_error(append_matrix(mat_1, 1),
               "This function only works with 'matrix' objects",
               fixed = TRUE)
  
  expect_error(append_matrix(mat_1, letters),
               "This function only works with 'matrix' objects",
               fixed = TRUE)
  
  expect_error(append_matrix(mat_1, data.frame(1, 2)),
               "This function only works with 'matrix' objects",
               fixed = TRUE)
  
  expect_error(append_matrix(mat_1, matrix(letters, ncol = 2)),
               "This function only works with numeric matrices",
               fixed = TRUE)
  
  expect_error(append_matrix(mat_1, matrix(c(TRUE, FALSE, TRUE, FALSE), 
                                           ncol = 2)),
               "This function only works with numeric matrices",
               fixed = TRUE)
  
  expect_error(append_matrix(mat_1, mat_1[-1, ]),
               paste0("Number of rows of matrices must be equal to number of ", 
                      "columns (connectivity matrix)"),
               fixed = TRUE)
  
  expect_error(append_matrix(mat_1, mat_1[ , -1]),
               paste0("Number of rows of matrices must be equal to number of ", 
                      "columns (connectivity matrix)"),
               fixed = TRUE)
  
  expect_error(append_matrix(mat_1, mat_bad_0),
               "Row names of matrices must contain node labels",
               fixed = TRUE)
  
  expect_error(append_matrix(mat_1, mat_bad_1),
               "Row names and column names of matrices must be equal",
               fixed = TRUE)
  
  expect_error(append_matrix(mat_1, mat_bad_2),
               "Some matrices do not contain any edges",
               fixed = TRUE)
})


# Tests for success ----

test_that("append_matrix() - Tests for success", {
  
  expect_silent({ check <- append_matrix(mat_1) })
  
  expect_true("matrix" %in% class(check))
  expect_equal(ncol(check), 3L)
  expect_equal(nrow(check), 3L)
  
  expect_true(colnames(check)[1] == colnames(mat_1)[1])
  expect_true(colnames(check)[2] == colnames(mat_1)[2])
  expect_true(colnames(check)[3] == colnames(mat_1)[3])
  
  expect_true(rownames(check)[1] == rownames(mat_1)[1])
  expect_true(rownames(check)[2] == rownames(mat_1)[2])
  expect_true(rownames(check)[3] == rownames(mat_1)[3])
  
  expect_equal(check[1, 1], 0L)
  expect_equal(check[2, 2], 1L)
  expect_equal(check[3, 3], 0L)
  
  
  expect_silent({ check <- append_matrix(mat_1, mat_2) })
  
  expect_true("matrix" %in% class(check))
  expect_equal(ncol(check), 6L)
  expect_equal(nrow(check), 6L)
  
  expect_true(colnames(check)[1] == colnames(mat_1)[1])
  expect_true(colnames(check)[2] == colnames(mat_1)[2])
  expect_true(colnames(check)[3] == colnames(mat_1)[3])
  expect_true(colnames(check)[4] == colnames(mat_2)[1])
  expect_true(colnames(check)[5] == colnames(mat_2)[2])
  expect_true(colnames(check)[6] == colnames(mat_2)[3])
  
  expect_true(rownames(check)[1] == rownames(mat_1)[1])
  expect_true(rownames(check)[2] == rownames(mat_1)[2])
  expect_true(rownames(check)[3] == rownames(mat_1)[3])
  expect_true(rownames(check)[4] == rownames(mat_2)[1])
  expect_true(rownames(check)[5] == rownames(mat_2)[2])
  expect_true(rownames(check)[6] == rownames(mat_2)[3])
  
  expect_equal(check[1, 1], 0L)
  expect_equal(check[2, 2], 1L)
  expect_equal(check[3, 3], 0L)
  expect_equal(check[4, 4], 0L)
  expect_equal(check[5, 5], 1L)
  expect_equal(check[6, 6], 0L)
  
  
  expect_silent({ check <- append_matrix(mat_2, mat_3) })
  
  expect_true("matrix" %in% class(check))
  expect_equal(ncol(check), 4L)
  expect_equal(nrow(check), 4L)
  
  expect_true(colnames(check)[1] == colnames(mat_3)[4])
  expect_true(colnames(check)[2] == colnames(mat_3)[1])
  expect_true(colnames(check)[3] == colnames(mat_3)[2])
  expect_true(colnames(check)[4] == colnames(mat_3)[3])
  
  expect_true(rownames(check)[1] == rownames(mat_3)[4])
  expect_true(rownames(check)[2] == rownames(mat_3)[1])
  expect_true(rownames(check)[3] == rownames(mat_3)[2])
  expect_true(rownames(check)[4] == rownames(mat_3)[3])
  
  expect_equal(sum(check), 15L)
  
  
  expect_silent({ check <- append_matrix(mat_2, mat_4, na_to_zero = TRUE) })
  
  expect_true("matrix" %in% class(check))
  expect_equal(ncol(check), 4L)
  expect_equal(nrow(check), 4L)
  
  expect_false(any(is.na(check)))
  
  
  expect_silent({ check <- append_matrix(mat_2, mat_4, na_to_zero = FALSE) })
  
  expect_true("matrix" %in% class(check))
  expect_equal(ncol(check), 4L)
  expect_equal(nrow(check), 4L)
  
  expect_true(any(is.na(check)))
})
