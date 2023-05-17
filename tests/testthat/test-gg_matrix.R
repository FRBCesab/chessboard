# Data for tests ----

mat <- matrix(data = sample(c(0, 1), 9, replace = TRUE), 
              nrow = 3, 
              ncol = 3)

colnames(mat) <- c("1-1", "1-2", "1-3")
rownames(mat) <- c("1-1", "1-2", "1-3")


# Tests for errors ----

test_that("gg_matrix() - Tests for errors", {
  
  expect_error(gg_matrix(),
               "Argument 'x' is required",
               fixed = TRUE)
  
  expect_error(gg_matrix(1),
               paste0("Argument 'x' must be a matrix (connectivity or ", 
                      "nodes-by-edges ", "matrix)"),
               fixed = TRUE)
  
  expect_error(gg_matrix(letters),
               paste0("Argument 'x' must be a matrix (connectivity or ", 
                      "nodes-by-edges ", "matrix)"),
               fixed = TRUE)
  
  expect_error(gg_matrix(data.frame(1, 2)),
               paste0("Argument 'x' must be a matrix (connectivity or ", 
                      "nodes-by-edges ", "matrix)"),
               fixed = TRUE)
  
  expect_error(gg_matrix(matrix(c(letters), ncol = 2)),
               paste0("Argument 'x' must be a numeric matrix (connectivity or ",
                      "nodes-by-edges matrix)"),
               fixed = TRUE)
  
  expect_error(gg_matrix(matrix(c(0, 1, 0, 2), ncol = 2)),
               "This function only works on binary matrix",
               fixed = TRUE)
  
  expect_error(gg_matrix(mat, title = 1),
               "Argument 'title' must be a character of length 1",
               fixed = TRUE)
  
  
  expect_error(gg_matrix(mat, title = c("Title", "Subtitle")),
               "Argument 'title' must be a character of length 1",
               fixed = TRUE)
})


# Tests for success ----

test_that("gg_matrix() - Tests for success", {

  expect_silent({ check <- gg_matrix(mat) })
  
  expect_true("ggplot" %in% class(check))
  expect_true("gg" %in% class(check))
  
  
  expect_silent({ check <- gg_matrix(mat, title = "Custom title") })
  
  expect_true("ggplot" %in% class(check))
  expect_true("gg" %in% class(check))
})
