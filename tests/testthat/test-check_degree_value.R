test_that("check_degree_value() - Tests for errors", {
  
  expect_error(check_degree_value(NULL),
               "Argument 'degree' is required", fixed = TRUE)
  
  expect_error(check_degree_value(letters),
               "Argument 'degree' must be a numeric", fixed = TRUE)
  
  expect_error(check_degree_value(data.frame(1:4, 4:7)),
               "Argument 'degree' must be a numeric", fixed = TRUE)
  
  expect_error(check_degree_value(NA),
               "Argument 'degree' must be a numeric", fixed = TRUE)
  
  expect_error(check_degree_value(matrix(1:9, ncol = 3)),
               "Argument 'degree' must be a numeric of length 1", fixed = TRUE)
  
  expect_error(check_degree_value(matrix(1:9, ncol = 1)),
               "Argument 'degree' must be a numeric of length 1", fixed = TRUE)
  
  expect_error(check_degree_value(1:2),
               "Argument 'degree' must be a numeric of length 1", fixed = TRUE)
  
  expect_error(check_degree_value(-10),
               "Argument 'degree' must be strictly positive", fixed = TRUE)
  
  expect_error(check_degree_value(0),
               "Argument 'degree' must be strictly positive", fixed = TRUE)
})


test_that("check_degree_value() - Tests for success", {

  expect_silent(check_degree_value(1))
  expect_null({check <- check_degree_value(10000) })
                
})
