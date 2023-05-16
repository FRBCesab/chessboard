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


test_that("gg_chessboard() - Tests for errors", {
  
  expect_error(gg_chessboard(),
               paste0("Argument 'nodes' is required ", 
                      "(output of the function create_nodes_labels())"),
               fixed = TRUE)
  
  expect_error(gg_chessboard(data.matrix(good_nodes)),
               paste0("Argument 'nodes' must be a data.frame ", 
                      "(output of the function create_nodes_labels())"),
               fixed = TRUE)
  
  expect_error(gg_chessboard(good_nodes[ , -1]),
               paste0("The column 'node' is absent from the 'nodes' ", 
                      "data.frame (output of the function ", 
                      "create_nodes_labels())"),
               fixed = TRUE)
  
  expect_error(gg_chessboard(good_nodes[ , -3]),
               paste0("The column 'transect' is absent from the 'nodes' ", 
                      "data.frame (output of the function ", 
                      "create_nodes_labels())"),
               fixed = TRUE)
  
  expect_error(gg_chessboard(good_nodes[ , -4]),
               paste0("The column 'quadrat' is absent from the 'nodes' ", 
                      "data.frame (output of the function ", 
                      "create_nodes_labels())"),
               fixed = TRUE)
  
  expect_error(gg_chessboard(good_nodes[1, ]),
               "Argument 'nodes' must have at least two rows (nodes)",
               fixed = TRUE)
  
  expect_error(gg_chessboard(bad_nodes_1),
               paste0("The column 'node' of the 'nodes' data.frame must be a ", 
               "character"),
               fixed = TRUE)
  
  expect_error(gg_chessboard(bad_nodes_2),
               paste0("The column 'transect' of the 'nodes' data.frame must ", 
                      "be a numeric"),
               fixed = TRUE)
  
  expect_error(gg_chessboard(bad_nodes_3),
               paste0("The column 'quadrat' of the 'nodes' data.frame must ", 
                      "be a numeric"),
               fixed = TRUE)
})

test_that("gg_chessboard() - Tests for success", {
  
  expect_silent({ check <- gg_chessboard(good_nodes) })
  
  expect_true("ggplot" %in% class(check))
  expect_true("gg" %in% class(check))
})
