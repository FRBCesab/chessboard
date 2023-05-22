# Data for tests ----

edges <- data.frame("from" = c("1-1", "1-2"), 
                    "to"   = c("1-2", "1-3"), 
                    "edge" = 1)

edges_2 <- data.frame("from" = c("1-1", "1-2", "1-1"), 
                      "to"   = c("1-2", "1-3", "1-1"), 
                      "edge" = c(1, 1, 0))

# Tests for errors ----

test_that("create_edges_id() - Tests for errors", {
  
  expect_error(create_edges_id(),
               "Argument 'edges' is required",
               fixed = TRUE)
  
  expect_error(create_edges_id(letters),
               "Argument 'edges' must be a data.frame",
               fixed = TRUE)
  
  expect_error(create_edges_id(data.matrix(letters)),
               "Argument 'edges' must be a data.frame",
               fixed = TRUE)
  
  expect_error(create_edges_id(edges[ , -1, drop = FALSE]),
               "The column 'from' is absent from the edges data.frame",
               fixed = TRUE)
  
  expect_error(create_edges_id(edges[ , -2, drop = FALSE]),
               "The column 'to' is absent from the edges data.frame",
               fixed = TRUE)
  
  expect_error(create_edges_id(edges[ , -3, drop = FALSE]),
               "The column 'edge' is absent from the edges data.frame",
               fixed = TRUE)
  
})


# Tests for success ----

test_that("create_edges_id() - Tests for success", {
  
  expect_silent({ check <- create_edges_id(edges) })
  
  expect_equal(class(check), "data.frame")
  expect_equal(ncol(check), 4L)
  expect_equal(nrow(check), 2L)
  expect_equal(colnames(check), c("edge_id", "edge", "from", "to"))
  expect_equal(check[1, 1], "E-1")
  expect_equal(check[2, 1], "E-2")
  
  expect_silent({ check <- create_edges_id(edges_2) })
  
  expect_equal(class(check), "data.frame")
  expect_equal(ncol(check), 4L)
  expect_equal(nrow(check), 3L)
  expect_equal(colnames(check), c("edge_id", "edge", "from", "to"))
  expect_equal(check[1, 1], "")  
  expect_equal(check[2, 1], "E-1")
  expect_equal(check[3, 1], "E-2")
  expect_equal(check[1, 2], 0L)
  expect_equal(check[2, 2], 1L)
  expect_equal(check[3, 2], 1L)
})
