# Data for tests ----

sites <- expand.grid("transect" = 1:3, 
                     "quadrat"  = 1:5)

nodes <- create_node_labels(data     = sites, 
                            transect = "transect", 
                            quadrat  = "quadrat")

focus <- "2-3"

neighbors <- pawn(nodes    = nodes, 
                  focus    = focus, 
                  degree   = 4, 
                  directed = FALSE, 
                  reverse  = TRUE)


# Tests for success ----

test_that("geom_edges() - Tests for success", {
  
  expect_silent({ check <- geom_edges(nodes, focus, neighbors) })
  
  expect_true("Layer" %in% class(check))
  expect_true("LayerInstance" %in% class(check))
  expect_true("ggproto" %in% class(check))
  expect_true("gg" %in% class(check))
})
