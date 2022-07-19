# Data for tests ----

tr_qr <- expand.grid("transect" = 1:3, "quadrat" = 1:5)
good_nodes <- create_nodes_labels(tr_qr, 
                                  transect = "transect", 
                                  quadrat  = "quadrat")
bad_nodes_1 <- good_nodes
bad_nodes_1$"node" <- 1:nrow(good_nodes)

bad_nodes_2 <- good_nodes
bad_nodes_2$"transect" <- as.character(bad_nodes_2$"transect")

bad_nodes_3 <- good_nodes
bad_nodes_3$"quadrat" <- as.character(bad_nodes_3$"quadrat")


test_that("check_nodes_object() - Tests for wrong inputs", {
  
  expect_error(check_nodes_object(),
               paste0("Argument 'nodes' is required ", 
                      "(output of the function create_nodes_labels())"),
               fixed = TRUE)
  
  expect_error(check_nodes_object(data.matrix(good_nodes)),
               paste0("Argument 'nodes' must be a data.frame ", 
                      "(output of the function create_nodes_labels())"),
               fixed = TRUE)
  
  expect_error(check_nodes_object(good_nodes[ , -1]),
               paste0("The column 'node' is absent from the 'nodes' data.frame ", 
                      "(output of the function create_nodes_labels())"),
               fixed = TRUE)
  
  expect_error(check_nodes_object(good_nodes[ , -3]),
               paste0("The column 'transect' is absent from the 'nodes' data.frame ", 
                      "(output of the function create_nodes_labels())"),
               fixed = TRUE)
  
  expect_error(check_nodes_object(good_nodes[ , -4]),
               paste0("The column 'quadrat' is absent from the 'nodes' data.frame ", 
                      "(output of the function create_nodes_labels())"),
               fixed = TRUE)
  
  expect_error(check_nodes_object(good_nodes[1, ]),
               "Argument 'nodes' must have at least two rows (nodes)",
               fixed = TRUE)
  
  expect_error(check_nodes_object(bad_nodes_1),
               "The column 'node' of the 'nodes' data.frame must be a character",
               fixed = TRUE)
  
  expect_error(check_nodes_object(bad_nodes_2),
               "The column 'transect' of the 'nodes' data.frame must be a numeric",
               fixed = TRUE)
  
  expect_error(check_nodes_object(bad_nodes_3),
               "The column 'quadrat' of the 'nodes' data.frame must be a numeric",
               fixed = TRUE)
  
})


test_that("check_nodes_object() - Tests for success", {
  
  expect_silent({ check <- check_nodes_object(good_nodes) })
  
  expect_null(check)
})