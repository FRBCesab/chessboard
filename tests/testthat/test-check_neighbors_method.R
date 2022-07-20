test_that("check_neighbors_method() - Tests for wrong inputs", {
  
  expect_error(check_neighbors_method(),
               "The argument 'method' is required",
               fixed = TRUE)
  
  expect_error(check_neighbors_method(NULL),
               "The argument 'method' cannot be NULL",
               fixed = TRUE)
  
  expect_error(check_neighbors_method(12),
               "The argument 'method' must be a character of length 1",
               fixed = TRUE)
  
  expect_error(check_neighbors_method(c("pawn", "fool")),
               "The argument 'method' must be a character of length 1",
               fixed = TRUE)
  
  expect_error(check_neighbors_method("Pawn"),
               paste0("Argument 'method' must be one of pawn, fool, ",
                      "rook, bishop, bishop_left, bishop_right, ", 
                      "knight, knight_left, knight_right, queen, ",
                      "wizard"),
               fixed = TRUE)
  
  expect_error(check_neighbors_method("knight-right"),
               paste0("Argument 'method' must be one of pawn, fool, ",
                      "rook, bishop, bishop_left, bishop_right, ", 
                      "knight, knight_left, knight_right, queen, ",
                      "wizard"),
               fixed = TRUE)
})


test_that("check_neighbors_method() - Tests for success", {
  
  expect_silent(check_neighbors_method("pawn"))
  expect_null({ check <- check_neighbors_method("knight_right") })
})
