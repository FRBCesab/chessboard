#' Combine several connectivity matrices
#'
#' @description 
#' Combines different connectivity matrices by row names and column names by
#' performing a 2-dimensional full join. Missing edges are filled with `0` 
#' (default) or `NA` (argument `na_to_zero`).
#'
#' @param ... one or several `matrix` objects created by 
#'   [connectivity_matrix()].
#' 
#' @param na_to_zero a `logical` value. If `TRUE` (default) missing edges are 
#'   coded as `0`. Otherwise they will be coded as `NA`.
#'
#' @return A connectivity matrix of dimensions `n x n`, where `n` is the total 
#'   number of unique nodes across all provided matrices.
#' 
#' @export
#'
#' @examples
#' mat1 <- matrix(rep(1, 9), nrow = 3)
#' colnames(mat1) <- c("A", "B", "C")
#' rownames(mat1) <- c("A", "B", "C")
#' mat1
#' 
#' mat2 <- matrix(rep(1, 9), nrow = 3)
#' colnames(mat2) <- c("D", "E", "F")
#' rownames(mat2) <- c("D", "E", "F")
#' mat2
#' 
#' mat3 <- matrix(rep(1, 9), nrow = 3)
#' colnames(mat3) <- c("F", "G", "H")
#' rownames(mat3) <- c("F", "G", "H")
#' mat3
#' 
#' append_matrix(mat1, mat2, mat3)
#' 
#' append_matrix(mat1, mat2, mat3, na_to_zero = FALSE)

append_matrix <- function(..., na_to_zero = TRUE) {
  
  ## Catch arguments ----
  
  matrices <- list(...)
  
  
  ## Check matrices ----
  
  if (length(matrices) == 0) {
    stop("Please provide at least one matrix", call. = FALSE) 
  }
  
  
  unlist(lapply(matrices, function(x) {
    
    if (!is.matrix(x)) {
      stop("This function only works with 'matrix' objects", 
           call. = FALSE)
    }
    
    if (!is.numeric(x)) {
      stop("This function only works with numeric matrices", 
           call. = FALSE)
    }
    
    if (nrow(x) != ncol(x)) {
      stop("Number of rows of matrices must be equal to number of columns ", 
           "(connectivity matrix)", 
           call. = FALSE)
    }
    
    if (is.null(rownames(x))) {
      stop("Row names of matrices must contain node labels", 
           call. = FALSE)
    }
    
    if (any(!(rownames(x) %in% colnames(x)))) {
      stop("Row names and column names of matrices must be equal", 
           call. = FALSE)
    }
    
    if (sum(x, na.rm = TRUE) == 0) {
      stop("Some matrices do not contain any edges", call. = FALSE)
    }
  }))
  
  
  ## Convert list of matrix to a single data.frame ----
  
  list_of_dfs <- lapply(matrices, function(x) {
    
    mat <- as.data.frame(x)
    mat <- data.frame(from = rownames(mat), mat, check.names = FALSE)
    
    tidyr::pivot_longer(mat, cols = -1, names_to = "to", values_to = "edge")
  })
  
  df <- do.call(rbind.data.frame, list_of_dfs)
  
  
  ## Order labels ----
  
  row_s <- sort(unique(df$"from"))
  col_s <- sort(unique(df$"to"))
  
  
  ## Convert to wide format ----
  
  mat <- tidyr::pivot_wider(df, names_from = "to", values_from = "edge", 
                            values_fn = max)
  
  
  ## Convert to matrix ----
  
  row_names <- mat[ , 1, drop = TRUE]
  mat <- data.matrix(mat[ , -1])
  rownames(mat) <- row_names
  
  
  ## Replace NA by 0 (if required) ----
  
  if (na_to_zero) {
    mat <- ifelse(is.na(mat), 0, mat)
  }
  
  
  ## Order matrix ----
  
  mat[row_s, col_s]
}
