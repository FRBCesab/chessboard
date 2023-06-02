# Data for tests ----

sites <- expand.grid("transect" = 1:3, 
                     "quadrat"  = 1:5)

nodes <- create_nodes_labels(data     = sites, 
                             transect = "transect", 
                             quadrat  = "quadrat")

focus <- "2-3"


# Tests for success ----

test_that("geom_node() - Tests for success", {
  
  expect_silent({ check <- geom_node(nodes, focus) })
  
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
