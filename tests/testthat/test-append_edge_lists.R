# Data for tests ----

edges     <- data.frame("from" = c("1-1", "1-2"), "to" = c("1-2", "1-3"))
edges_bis <- data.frame("from" = c("2-1", "2-2"), "to" = c("2-2", "2-3"))
edges_ter <- data.frame("from" = c("1-1", "2-2"), "to" = c("1-2", "2-3"))

test_that("append_edge_lists() - Tests for errors", {
  
  expect_error(append_edge_lists(),
               "Please provide at least one edge list",
               fixed = TRUE)
})


test_that("append_edge_lists() - Tests for success", {
  
  # One edges list ----
  
  expect_silent({ edges_l <- append_edge_lists(edges) })
  
  expect_equal(class(edges_l), "data.frame")
  expect_equal(ncol(edges_l), 2L)
  expect_equal(nrow(edges_l), 2L)
  expect_equal(colnames(edges_l), c("from", "to"))
  expect_equal(edges_l, edges)

  
  # Two duplicated edges lists ----
  
  expect_silent({ edges_l <- append_edge_lists(edges, edges) })
  
  expect_equal(class(edges_l), "data.frame")
  expect_equal(ncol(edges_l), 2L)
  expect_equal(nrow(edges_l), 2L)
  expect_equal(colnames(edges_l), c("from", "to"))
  expect_equal(edges_l, edges)
  
  
  # Two different edges lists ----
  
  expect_silent({ edges_l <- append_edge_lists(edges, edges_bis) })
  
  expect_equal(class(edges_l), "data.frame")
  expect_equal(ncol(edges_l), 2L)
  expect_equal(nrow(edges_l), 4L)
  expect_equal(colnames(edges_l), c("from", "to"))
  expect_equal(edges_l, rbind(edges, edges_bis))

  
  # Two different edges lists w/ duplicates ----
  
  expect_silent({ edges_l <- append_edge_lists(edges, edges_ter) })
  
  expect_equal(class(edges_l), "data.frame")
  expect_equal(ncol(edges_l), 2L)
  expect_equal(nrow(edges_l), 3L)
  expect_equal(colnames(edges_l), c("from", "to"))
})
