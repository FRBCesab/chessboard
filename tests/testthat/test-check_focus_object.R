# Data for tests ----

nodes <- create_node_labels(expand.grid("transect" = 1:3, "quadrat" = 1:5), 
                             transect = "transect", 
                             quadrat  = "quadrat")


test_that("check_focus_object() - Tests for wrong inputs", {
  
  expect_error(check_focus_object(),
               paste0("Argument 'nodes' is required ", 
                      "(output of the function create_node_labels())"),
               fixed = TRUE)
  
  expect_error(check_focus_object(nodes),
               "Argument 'focus' is required (node label)",
               fixed = TRUE)
  
  expect_error(check_focus_object(nodes, 1),
               "Argument 'focus' must be a character (node label)",
               fixed = TRUE)
  
  expect_error(check_focus_object(nodes, letters[1:2]),
               "Argument 'focus' must be a character of length 1 (node label)",
               fixed = TRUE)
  
  expect_error(check_focus_object(nodes, "5-5"),
               paste0("The node '5-5' is absent from the nodes list ", 
                      "(argument 'nodes')"),
               fixed = TRUE)
  
})


test_that("check_focus_object() - Tests for success", {
  
  expect_silent(check_focus_object(nodes, "2-3"))
  expect_null({ check <- check_focus_object(nodes, "2-3") })
})
