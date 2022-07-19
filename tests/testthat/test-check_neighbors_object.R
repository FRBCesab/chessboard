# Data for tests ----

nodes <- create_nodes_labels(expand.grid("transect" = 1:9, "quadrat" = 1:9), 
                             transect = "transect", 
                             quadrat  = "quadrat")

focus     <- "5-5"

neighbors <- pawn(nodes, focus)

bad_neighbors_1            <- neighbors
bad_neighbors_1$"node"     <- 1:nrow(neighbors)

bad_neighbors_2            <- neighbors
bad_neighbors_2$"transect" <- as.character(bad_neighbors_2$"transect")

bad_neighbors_3           <- neighbors
bad_neighbors_3$"quadrat" <- as.character(bad_neighbors_3$"quadrat")

test_that("check_neighbors_object() - Tests for wrong inputs", {
  
  expect_error(check_neighbors_object(),
               paste0("Argument 'neighbors' is required ", 
                      "(output of the functions pawn(), fool(), etc.)"),
               fixed = TRUE)
  
  expect_error(check_neighbors_object(data.matrix(neighbors)),
               paste0("Argument 'neighbors' must be a data.frame (output of ", 
               "the functions pawn(), fool(), etc."),
               fixed = TRUE)
  
  expect_error(check_neighbors_object(neighbors[ , -1]),
               paste0("The column 'node' is absent from the 'neighbors' ", 
                      "data.frame (output of the functions pawn(), fool(), ", 
                      "etc.)"),
               fixed = TRUE)
  
  expect_error(check_neighbors_object(neighbors[ , -3]),
               paste0("The column 'transect' is absent from the 'neighbors' ", 
                      "data.frame (output of the functions pawn(), fool(), ", 
                      "etc.)"),
               fixed = TRUE)
  
  expect_error(check_neighbors_object(neighbors[ , -4]),
               paste0("The column 'quadrat' is absent from the 'neighbors' ", 
                      "data.frame (output of the functions pawn(), fool(), ", 
                      "etc.)"),
               fixed = TRUE)
  
  expect_error(check_neighbors_object(neighbors[0, ]),
               "Argument 'neighbors' must have at least one row (neighbor)",
               fixed = TRUE)
  
  expect_error(check_neighbors_object(bad_neighbors_1),
               paste0("The column 'node' of the 'neighbors' data.frame must ", 
                      "be a character"),
               fixed = TRUE)
  
  expect_error(check_neighbors_object(bad_neighbors_2),
               paste0("The column 'transect' of the 'neighbors' data.frame ", 
                      "must be a numeric"),
               fixed = TRUE)
  
  expect_error(check_neighbors_object(bad_neighbors_3),
               paste0("The column 'quadrat' of the 'neighbors' data.frame", 
                      " must be a numeric"),
               fixed = TRUE)
})


test_that("check_neighbors_object() - Tests for success", {
  
  expect_silent(check_neighbors_object(neighbors))
  expect_null({ check <- check_neighbors_object(neighbors) })
})
