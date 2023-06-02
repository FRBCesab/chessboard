# Data for tests ----

edges <- data.frame("from" = c("1-1", "1-2"), "to" = c("1-2", "1-3"))
edges_bad_1 <- data.frame("from" = 1:3, "to" = as.character(1:3))
edges_bad_2 <- data.frame("from" = as.character(1:3), "to" = 1:3)

test_that("check_edges_object() - Tests for wrong inputs", {
  
  expect_error(check_edges_object(),
               paste0("Argument 'edges' is required ", 
                      "(output of the function create_edge_list())"),
               fixed = TRUE)
  
  expect_error(check_edges_object(letters),
               paste0("Argument 'edges' must be a data.frame ", 
                      "(output of the function create_edge_list())"),
               fixed = TRUE)
  
  expect_error(check_edges_object(data.matrix(letters)),
               paste0("Argument 'edges' must be a data.frame ", 
                      "(output of the function create_edge_list())"),
               fixed = TRUE)
  
  expect_error(check_edges_object(data.frame(letters)),
               paste0("The column 'from' is absent from the 'edges' ", 
                      "data.frame (output of the function ", 
                      "create_edge_list())"),
               fixed = TRUE)
  
  expect_error(check_edges_object(edges[ , -1, drop = FALSE]),
               paste0("The column 'from' is absent from the 'edges' ", 
                      "data.frame (output of the function ", 
                      "create_edge_list())"),
               fixed = TRUE)
  
  expect_error(check_edges_object(edges[ , -2, drop = FALSE]),
               paste0("The column 'to' is absent from the 'edges' ", 
                      "data.frame (output of the function ", 
                      "create_edge_list())"),
               fixed = TRUE)
  
  expect_error(check_edges_object(edges[0, ]),
               paste0("Argument 'edges' must have at least one row (edge)"),
               fixed = TRUE)
  
  expect_error(check_edges_object(edges_bad_1),
               paste0("The column 'from' of the 'edges' data.frame must be a ",
                      "character"),
               fixed = TRUE)
  
  expect_error(check_edges_object(edges_bad_2),
               paste0("The column 'to' of the 'edges' data.frame must be a ",
                      "character"),
               fixed = TRUE)
})


test_that("check_edges_object() - Tests for success", {
  
  expect_silent({ check <- check_edges_object(edges) })
  
  expect_null(check)
})
