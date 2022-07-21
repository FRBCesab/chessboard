# Data for tests ----

edges     <- data.frame("from" = c("1-1", "1-2"), "to" = c("1-2", "1-3"))

test_that("get_edges_transects_labels() - Tests for errors", {
  
  expect_error(get_edges_transects_labels(),
               paste0("Argument 'edges' is required (output of the function ", 
                      "create_edges_list())"),
               fixed = TRUE)
})


test_that("get_edges_transects_labels() - Tests for success", {
  
  expect_silent({ transects_labels <- get_edges_transects_labels(edges) })
  
  expect_equal(class(transects_labels), "data.frame")
  
  expect_equal(ncol(transects_labels), 4L)
  expect_equal(nrow(transects_labels), 2L)
  expect_equal(colnames(transects_labels), 
               c("from", "to", "transects_from", "transects_to"))
  expect_true(is.numeric(transects_labels$"transects_from"))
  expect_true(is.numeric(transects_labels$"transects_to"))
  expect_equal(transects_labels[1, 1], "1-1")
  expect_equal(transects_labels[1, 2], "1-2")
  expect_equal(transects_labels[1, 3], 1L)
  expect_equal(transects_labels[1, 4], 1L)
})

