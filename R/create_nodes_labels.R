#' Create unique nodes labels 
#'
#' @description 
#' Creates unique nodes (sampling units) labels in directed (or undirected) 
#' spatial (or not) networks. 
#' 
#' It's important to note that, even the package `chessboard` is designed to 
#' deal with spatial networks, it does not explicitly use spatial coordinates.
#' Every functions of the package will use the **nodes labels**.
#' 
#' To work, the package `chessboard` requires that the sampling has two
#' dimensions: 
#' one from bottom to top (called **quadrats**), and one from left to right 
#' (called **transects**). If the sampling has been conducted along one single 
#' dimension (**transects** or **quadrats**), this function will create a 
#' fictitious label for the missing dimension.
#' In other words, the package `chessboard` can work with sampling designs such
#' as regular grids (two dimensions), transects (one dimension), and quadrats 
#' (one dimension).
#' 
#' In addition, the package can also deal with multiple locations. In that 
#' case, users will need to use the argument `location`.
#' 
#' The nodes labels will be of the form: `1-2`, where `1` is the identifier of 
#' the transect (created by the function if missing), and `2`, the identifier 
#' of the quadrat (created by the function if missing).
#' 
#' @param data a `data.frame` with at least one column, `'transect'` or 
#'   `'quadrat'`. If only one column is provided and `transect` or `quadrat` 
#'   is `NULL`, the network will be considered as one-dimensional. If `data` 
#'   contains both `'transect'` and `'quadrat'` columns, the network will be 
#'   considered as two-dimensional. The `data.frame` can contain additional
#'   columns.
#' 
#' @param location a `character` of length 1. The name of the column that 
#'   contains location identifiers. If missing (or `NULL`), a unique location
#'   identifier will be created and named `1` (for the purpose of the package 
#'   only). This argument is optional if the sampling ha been conducted at one
#'   location, but required if the survey is structured in multiple locations.
#' 
#' @param transect a `character` of length 1. The name of the column that 
#'   contains transect identifiers. If missing (or `NULL`), a unique transect
#'   identifier will be created and named `1` (for the purpose of the package 
#'   only). If missing, the network will be considered as one-dimensional.
#' 
#' @param quadrat a `character` of length 1. The name of the column that 
#'   contains quadrat identifiers. If missing (or `NULL`), a unique quadrat
#'   identifier will be created and named `1` (for the purpose of the package 
#'   only). If missing, the network will be considered as one-dimensional.
#'
#' @return A `data.frame` with at least the four following columns:
#' - `node`, the nodes label
#' - `location`, the identifier of the location
#' - `transect`, the identifier of the transect
#' - `quadrat`, the identifier of the quadrat
#' Other columns present in the original dataset will also be added.
#' 
#' @export
#' 
#' @examples 
#' library("chessboard")
#' 
#' # Two-dimensional sampling ----
#' sites_infos <- expand.grid("transect" = 1:3, "quadrat" = 1:5)
#' sites_infos
#' 
#' nodes <- create_nodes_labels(data     = sites_infos, 
#'                              transect = "transect", 
#'                              quadrat  = "quadrat")
#' nodes
#' 
#' gg_chessboard(nodes)
#' 
#' # One-dimensional sampling (only transects) ----
#' transects_only <- data.frame("transect" = 1:5)
#' 
#' nodes <- create_nodes_labels(transects_only,
#'                              transect = "transect")
#' nodes
#' 
#' gg_chessboard(nodes)
#' 
#' # One-dimensional sampling (only quadrats) ----
#' quadrats_only <- data.frame("quadrat" = 1:5)
#' 
#' nodes <- create_nodes_labels(quadrats_only,
#'                              quadrat = "quadrat")
#' nodes
#' 
#' gg_chessboard(nodes)

create_nodes_labels <- function(data, location, transect, quadrat) {
  
  if (missing(data)) {
    stop("The argument 'data' is required", call. = FALSE)
  }
  
  if (!is.data.frame(data)) {
    stop("The argument 'data' must be data.frame", call. = FALSE)
  }
  
  if (nrow(data) < 1) {
    stop("The data.frame 'data' must have at least one row", call. = FALSE)
  }
  
  
  if (missing(location)) location <- NULL
  if (missing(transect)) transect <- NULL
  if (missing(quadrat))  quadrat  <- NULL
  
  
  if (is.null(transect) && is.null(quadrat)) {
    stop("Please provide at least either 'transect' or 'quadrat'", 
         call. = FALSE)
  }
  
  if (!is.null(location)) {
    
    if (!is.character(location) || length(location) != 1) {
      stop("Argument 'location' must be a character of length 1 ", 
           "(column name of the locations)", call. = FALSE)
    }
    
    if (!(location %in% colnames(data))) {
      stop(paste0("The column '", location, "' is absent from 'data'"), 
           call. = FALSE)
    }
    
    if (!is.numeric(data[ , location])) {
      stop(paste0("The column '", location, "' must be a numeric"), 
           call. = FALSE)
    }
  }
  
  if (!is.null(transect)) {
    
    if (!is.character(transect) || length(transect) != 1) {
      stop("Argument 'transect' must be a character of length 1 ", 
           "(column name of the transects)", call. = FALSE)
    }
    
    if (!(transect %in% colnames(data))) {
      stop(paste0("The column '", transect, "' is absent from 'data'"), 
           call. = FALSE)
    }
    
    if (!is.numeric(data[ , transect])) {
      stop(paste0("The column '", transect, "' must be a numeric"), 
           call. = FALSE)
    }
  }
  
  if (!is.null(quadrat)) {
    
    if (!is.character(quadrat) || length(quadrat) != 1) {
      stop("Argument 'quadrat' must be a character of length 1 ", 
           "(column name of the quadrats)", call. = FALSE)
    }
    
    if (!(quadrat %in% colnames(data))) {
      stop(paste0("The column '", quadrat, "' is absent from 'data'"), 
           call. = FALSE)
    }
    
    if (!is.numeric(data[ , quadrat])) {
      stop(paste0("The column '", quadrat, "' must be a numeric"), 
           call. = FALSE)
    }
  }
  
  if (is.null(transect)) {

    if (any(duplicated(data[ , quadrat]))) {
      stop(paste0("As argument 'transect' is not provided, the column '", 
                  quadrat, "' cannot contain duplicated values"), 
           call. = FALSE)
    }

    data[ , "transect"] <- rep(1, nrow(data))
  }

  if (is.null(quadrat)) {
    
    if (any(duplicated(data[ , transect]))) {
      stop(paste0("As argument 'quadrat' is not provided, the column '", 
                  transect, "' cannot contain duplicated values"), 
           call. = FALSE)
    }
    
    data[ , "quadrat"] <- rep(1, nrow(data))
  }
  

  if (is.null(location)) {
    data[ , "location"] <- rep(1, nrow(data))
  }
  
  if (any(is.na(data[ , transect]))) {
    stop(paste0("The column '", transect, "' cannot contain NA"), 
         call. = FALSE)
  }
  
  if (any(is.na(data[ , quadrat]))) {
    stop(paste0("The column '", quadrat, "' cannot contain NA"), 
         call. = FALSE)
  }

  
  ## Rename columns ----
  
  if (!is.null(location)) {
    colnames(data)[which(colnames(data) == location)] <- "location"
  }
  
  if (!is.null(transect)) {
    colnames(data)[which(colnames(data) == transect)] <- "transect"
  }
  
  if (!is.null(quadrat)) {
    colnames(data)[which(colnames(data) == quadrat)]  <- "quadrat"
  }
  
  
  ## Check labels ----
  ## ... Disable this to deal w/ complex networks ...
  
  # if (max(diff(sort(data[ , "transect"]))) > 1) {
  #   stop("Labels of transects must increased by 1 (no gap allowed in labels)", 
  #        call. = FALSE)
  # }
  # 
  # if (max(diff(sort(data[ , "quadrat"]))) > 1) {
  #   stop("Labels of quadrats must increased by 1 (no gap allowed in labels)", 
  #        call. = FALSE)
  # }
  
  
  ## Create nodes labels ----
  
  nodes <- data[with(data, order(location, transect, quadrat)), ]
  rownames(nodes) <- NULL

  transects_labels <- format(nodes$"transect")
  transects_labels <- gsub("\\s", "0", transects_labels)
  
  quadrats_labels <- format(nodes$"quadrat")
  quadrats_labels <- gsub("\\s", "0", quadrats_labels)
  
  keys <- paste0(transects_labels, "-", quadrats_labels)
  
  if (any(duplicated(keys))) {
    stop("Nodes labels cannot contain duplicates", call. = FALSE)
  }
  
  
  ## Check for irregular grid ----
  
  # if (nrow(nodes) < 
  #     (length(unique(nodes$"transect")) * length(unique(nodes$"quadrat")))) {
  #   stop("The package 'chessboard' is not designed to work with irregular ", 
  #        "grids", call. = FALSE)
  # }
  
  
  ## Order columns ----
  
  col_to_add <- which(colnames(nodes) %in% c("location", "transect", "quadrat"))
  
  data <- data.frame("node" = keys, 
                     "location" = nodes$"location",
                     "transect" = nodes$"transect",
                     "quadrat"  = nodes$"quadrat")
  
  if (length(col_to_add) < ncol(nodes)) {
    data <- cbind(data, nodes[ , -col_to_add, drop = FALSE])
  }
  
  data
}
