# Data for tests ----

edges     <- data.frame("from" = c("1-1", "1-2"), "to" = c("1-2", "1-3"))

test_that("get_edges_quadrats_labels() - Tests for errors", {
  
  expect_error(get_edges_quadrats_labels(),
              paste0("Argument 'edges' is required (output of the function ", 
                     "create_edges_list())"),
               fixed = TRUE)
})


test_that("get_edges_quadrats_labels() - Tests for success", {
  
  expect_silent({ quadrats_labels <- get_edges_quadrats_labels(edges) })
  
  expect_equal(class(quadrats_labels), "data.frame")
  
  expect_equal(ncol(quadrats_labels), 4L)
  expect_equal(nrow(quadrats_labels), 2L)
  expect_equal(colnames(quadrats_labels), 
               c("from", "to", "quadrats_from", "quadrats_to"))
  expect_true(is.numeric(quadrats_labels$"quadrats_from"))
  expect_true(is.numeric(quadrats_labels$"quadrats_to"))
  expect_equal(quadrats_labels[1, 1], "1-1")
  expect_equal(quadrats_labels[1, 2], "1-2")
  expect_equal(quadrats_labels[1, 3], 1L)
  expect_equal(quadrats_labels[1, 4], 2L)
})

