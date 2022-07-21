edges <- data.frame("from" = c("1-2", "1-1"), "to" = c("1-3", "1-2"))

test_that("get_sorted_nodes() - Tests for errors", {

  expect_error(get_sorted_nodes(),
               paste0("Argument 'edges' is required (output of the function ", 
                      "create_edges_list())"),
               fixed = TRUE)
})

test_that("get_sorted_nodes() - Tests for success", {

  expect_silent({ ordered_nodes <- get_sorted_nodes(edges) })
  
  expect_true(is.character(ordered_nodes))
  expect_true(length(ordered_nodes) == 3L)
  expect_equal(ordered_nodes, c("1-1", "1-2", "1-3"))
})
