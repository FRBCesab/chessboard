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

nodes <- create_nodes_labels(expand.grid("transect" = 1:3, "quadrat" = 1:5), 
                             transect = "transect", 
                             quadrat  = "quadrat")

test_that("geom_piece() - Tests for errors", {
  
  expect_error(geom_piece(),
               paste0("Argument 'nodes' is required ", 
                      "(output of the function create_nodes_labels())"),
               fixed = TRUE)
  
  expect_error(geom_piece(data.matrix(good_nodes)),
               paste0("Argument 'nodes' must be a data.frame ", 
                      "(output of the function create_nodes_labels())"),
               fixed = TRUE)
  
  expect_error(geom_piece(good_nodes[ , -1]),
               paste0("The column 'node' is absent from the 'nodes' ", 
                      "data.frame (output of the function ", 
                      "create_nodes_labels())"),
               fixed = TRUE)
  
  expect_error(geom_piece(good_nodes[ , -3]),
               paste0("The column 'transect' is absent from the 'nodes' ", 
                      "data.frame (output of the function ", 
                      "create_nodes_labels())"),
               fixed = TRUE)
  
  expect_error(geom_piece(good_nodes[ , -4]),
               paste0("The column 'quadrat' is absent from the 'nodes' ", 
                      "data.frame (output of the function ", 
                      "create_nodes_labels())"),
               fixed = TRUE)
  
  expect_error(geom_piece(good_nodes[1, ]),
               "Argument 'nodes' must have at least two rows (nodes)",
               fixed = TRUE)
  
  expect_error(geom_piece(bad_nodes_1),
               paste0("The column 'node' of the 'nodes' data.frame must be a ", 
                      "character"),
               fixed = TRUE)
  
  expect_error(geom_piece(bad_nodes_2),
               paste0("The column 'transect' of the 'nodes' data.frame must ", 
                      "be a numeric"),
               fixed = TRUE)
  
  expect_error(geom_piece(bad_nodes_3),
               paste0("The column 'quadrat' of the 'nodes' data.frame must ", 
                      "be a numeric"),
               fixed = TRUE)
  
  expect_error(geom_piece(nodes),
               "Argument 'focus' is required (node label)",
               fixed = TRUE)
  
  expect_error(geom_piece(nodes, 1),
               "Argument 'focus' must be a character (node label)",
               fixed = TRUE)
  
  expect_error(geom_piece(nodes, letters[1:2]),
               "Argument 'focus' must be a character of length 1 (node label)",
               fixed = TRUE)
  
  expect_error(geom_piece(nodes, "5-5"),
               paste0("The node '5-5' is absent from the nodes list ", 
                      "(argument 'nodes')"),
               fixed = TRUE)
})

test_that("geom_piece() - Tests for success", {
  
  expect_silent({ check <- geom_piece(nodes, "1-3") })
  
  expect_true("list" %in% class(check))
  expect_equal(length(check), 3L)
  
  expect_true("Layer" %in% class(check[[1]]))
  expect_true("LayerInstance" %in% class(check[[1]]))
  expect_true("ggproto" %in% class(check[[1]]))
  expect_true("gg" %in% class(check[[1]]))
  
  expect_true("Layer" %in% class(check[[2]]))
  expect_true("LayerInstance" %in% class(check[[2]]))
  expect_true("ggproto" %in% class(check[[2]]))
  expect_true("gg" %in% class(check[[2]]))
  
  expect_true("Layer" %in% class(check[[3]]))
  expect_true("LayerInstance" %in% class(check[[3]]))
  expect_true("ggproto" %in% class(check[[3]]))
  expect_true("gg" %in% class(check[[3]]))
})
