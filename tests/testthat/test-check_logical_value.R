test_that("check_logical_value() - Tests for wrong inputs", {
  
  expect_error(check_logical_value(NULL),
               "The argument 'NULL' cannot be NULL",
               fixed = TRUE)
  
  expect_error(check_logical_value(12),
               paste0("The argument '12' must be a logical (TRUE or FALSE) ", 
               "of length 1"),
               fixed = TRUE)
  
  expect_error(check_logical_value(c(TRUE, FALSE)),
               paste0("The argument 'c(TRUE, FALSE)' must be a logical ", 
                      "(TRUE or FALSE) of length 1"),
               fixed = TRUE)
  
  expect_error(check_logical_value(NA),
               "The argument 'NA' cannot be NA",
               fixed = TRUE)
})


test_that("check_logical_value() - Tests for success", {
  
  expect_silent(check_logical_value(TRUE))
  expect_null({ check <- check_logical_value(FALSE) })
})
