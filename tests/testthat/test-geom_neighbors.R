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


test_that("geom_neighbors() - Tests for errors", {
  
  expect_error(geom_neighbors(),
               paste0("Argument 'nodes' is required ", 
                      "(output of the function create_nodes_labels())"),
               fixed = TRUE)
  
  expect_error(geom_neighbors(data.matrix(good_nodes)),
               paste0("Argument 'nodes' must be a data.frame ", 
                      "(output of the function create_nodes_labels())"),
               fixed = TRUE)
  
  expect_error(geom_neighbors(good_nodes[ , -1]),
               paste0("The column 'node' is absent from the 'nodes' ", 
                      "data.frame (output of the function ", 
                      "create_nodes_labels())"),
               fixed = TRUE)
  
  expect_error(geom_neighbors(good_nodes[ , -3]),
               paste0("The column 'transect' is absent from the 'nodes' ", 
                      "data.frame (output of the function ", 
                      "create_nodes_labels())"),
               fixed = TRUE)
  
  expect_error(geom_neighbors(good_nodes[ , -4]),
               paste0("The column 'quadrat' is absent from the 'nodes' ", 
                      "data.frame (output of the function ", 
                      "create_nodes_labels())"),
               fixed = TRUE)
  
  expect_error(geom_neighbors(good_nodes[1, ]),
               "Argument 'nodes' must have at least two rows (nodes)",
               fixed = TRUE)
  
  expect_error(geom_neighbors(bad_nodes_1),
               paste0("The column 'node' of the 'nodes' data.frame must be a ", 
                      "character"),
               fixed = TRUE)
  
  expect_error(geom_neighbors(bad_nodes_2),
               paste0("The column 'transect' of the 'nodes' data.frame must ", 
                      "be a numeric"),
               fixed = TRUE)
  
  expect_error(geom_neighbors(bad_nodes_3),
               paste0("The column 'quadrat' of the 'nodes' data.frame must ", 
                      "be a numeric"),
               fixed = TRUE)
  
  expect_error(geom_neighbors(nodes, ),
               paste0("Argument 'neighbors' is required ", 
                      "(output of the functions pawn(), fool(), etc.)"),
               fixed = TRUE)
  
  expect_error(geom_neighbors(nodes, data.matrix(neighbors)),
               paste0("Argument 'neighbors' must be a data.frame (output of ", 
                      "the functions pawn(), fool(), etc."),
               fixed = TRUE)
  
  expect_error(geom_neighbors(nodes, neighbors[ , -1]),
               paste0("The column 'node' is absent from the 'neighbors' ", 
                      "data.frame (output of the functions pawn(), fool(), ", 
                      "etc.)"),
               fixed = TRUE)
  
  expect_error(geom_neighbors(nodes, neighbors[ , -3]),
               paste0("The column 'transect' is absent from the 'neighbors' ", 
                      "data.frame (output of the functions pawn(), fool(), ", 
                      "etc.)"),
               fixed = TRUE)
  
  expect_error(geom_neighbors(nodes, neighbors[ , -4]),
               paste0("The column 'quadrat' is absent from the 'neighbors' ", 
                      "data.frame (output of the functions pawn(), fool(), ", 
                      "etc.)"),
               fixed = TRUE)
  
  expect_error(geom_neighbors(nodes, neighbors[0, ]),
               "Argument 'neighbors' must have at least one row (neighbor)",
               fixed = TRUE)
  
  expect_error(geom_neighbors(nodes, bad_neighbors_1),
               paste0("The column 'node' of the 'neighbors' data.frame must ", 
                      "be a character"),
               fixed = TRUE)
  
  expect_error(geom_neighbors(nodes, bad_neighbors_2),
               paste0("The column 'transect' of the 'neighbors' data.frame ", 
                      "must be a numeric"),
               fixed = TRUE)
  
  expect_error(geom_neighbors(nodes, bad_neighbors_3),
               paste0("The column 'quadrat' of the 'neighbors' data.frame", 
                      " must be a numeric"),
               fixed = TRUE)
})

test_that("geom_neighbors() - Tests for success", {

  expect_silent({ check <- geom_neighbors(nodes, neighbors) })
  
  expect_true("Layer" %in% class(check))
  expect_true("LayerInstance" %in% class(check))
  expect_true("ggproto" %in% class(check))
  expect_true("gg" %in% class(check))
})
