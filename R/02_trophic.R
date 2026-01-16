#' Create an empty trophic table
#'
#' Creates an empty data.frame of class \code{trophic_tbl} designed
#' to store trophic links in a safe and structured way.
#'
#' @return An empty object of class \code{trophic_tbl} with columns:
#' \describe{
#'   \item{link}{A list column containing named character vectors c(from=..., to=...)}
#'   \item{weight}{Numeric weight associated with each link}
#' }
#' The object also contains an attribute \code{level} that will store
#' the trophic level of each node.
#'
#' @examples
#' net <- trophic()
#'
#' @export
trophic <- function() {
  df <- data.frame(
    link = I(list()),
    weight = numeric(),
    stringsAsFactors = FALSE
  )

  class(df) <- c("trophic_tbl", class(df))
  attr(df, "level") <- numeric()

  return(df)
}


#' Add links to a trophic table
#'
#' Adds one or several directed links to a trophic_tbl object.
#'
#' @param tbl A \code{trophic_tbl} object.
#' @param from A single character string indicating the source node.
#' @param to A character vector indicating target nodes.
#' @param weight A numeric vector of weights associated with each link.
#'   If a single value is provided, it is recycled to match the length of \code{to}.
#'
#' @details
#' The function performs several checks:
#' \itemize{
#'   \item \code{from} must be a scalar character string
#'   \item \code{to} must be a character vector
#'   \item Links must be unique
#'   \item Self-loops (from == to) are forbidden
#'   \item The resulting graph must remain acyclic
#' }
#'
#' @return A validated \code{trophic_tbl} object with the new links added.
#'
#' @examples
#' net <- trophic() |>
#'   add_link("a", "b", weight = 1)
#'
#' @export
add_link <- function(tbl, from, to, weight = 1) {

  stopifnot(inherits(tbl, "trophic_tbl"))
  stopifnot(is.character(from), length(from) == 1)
  stopifnot(is.character(to))

  if (length(weight) == 1) {
    weight <- rep(weight, length(to))
  }

  stopifnot(length(to) == length(weight))

  new_links <- mapply(function(t, w) {

    stopifnot(from != t)

    structure(
      c(from = from, to = t),
      class = "trophic_link"
    )

  }, to, weight, SIMPLIFY = FALSE)

  new_df <- data.frame(
    link = I(new_links),
    weight = weight,
    stringsAsFactors = FALSE
  )

  result <- rbind(tbl, new_df)

  validate_trophic_tbl(result)
}

#' Validate a trophic table
#'
#' Internal function used to ensure that a trophic_tbl object respects
#' all structural constraints.
#'
#' @param df A data.frame intended to be a trophic_tbl object.
#'
#' @details
#' The function checks that:
#' \itemize{
#'   \item All links are unique
#'   \item No self-loops are present
#'   \item The graph is acyclic
#' }
#' It also computes and updates the trophic level attribute.
#'
#' @return A validated \code{trophic_tbl} object.
#'
#' @keywords internal
validate_trophic_tbl <- function(df) {

  # 1. check unique link
  link_strings <- vapply(df$link, function(x) {
    paste(x["from"], x["to"], sep = "->")
  }, character(1))
  if (any(duplicated(link_strings))) {
    stop("Links must be unique")
  }

  # 2. check self loop
  for (l in df$link) {
    if (l["from"] == l["to"]) {
      stop("Self loops are not allowed")
    }
  }

  # 3. check acyclicity
  edges <- data.frame(
    from = sapply(df$link, `[[`, "from"),
    to   = sapply(df$link, `[[`, "to"),
    stringsAsFactors = FALSE
  )
  if (is_cyclic(edges)) {
    stop("The trophic graph must be acyclic")
  }

  # 4. add trophic level
  levels <- compute_levels(edges)
  attr(df, "level") <- levels

  # 5. specify class and return
  class(df) <- c("trophic_tbl", "data.frame")
  df
}

#' Test if a directed graph is cyclic
#'
#' Implements Kahn's algorithm to detect cycles in a directed graph.
#'
#' @param df A data.frame with columns \code{from} and \code{to}.
#'
#' @return Logical. TRUE if the graph contains at least one cycle.
#'
#' @examples
#' df <- data.frame(from=c("A","B"), to=c("B","A"))
#' is_cyclic(df)
#'
#' @export
is_cyclic <- function(df) {

  nodes <- unique(c(df$from, df$to))

  indegree <- setNames(rep(0, length(nodes)), nodes)
  for (v in df$to) {
    indegree[v] <- indegree[v] + 1
  }

  queue <- names(indegree[indegree == 0])

  edges <- df[, c("from", "to")]

  count <- 0

  while (length(queue) > 0) {
    node <- queue[1]
    queue <- queue[-1]

    count <- count + 1

    outgoing <- edges[edges$from == node, ]

    for (v in outgoing$to) {
      indegree[v] <- indegree[v] - 1

      if (indegree[v] == 0) {
        queue <- c(queue, v)
      }
    }

    edges <- edges[edges$from != node, ]
  }

  count != length(nodes)
}


#' Compute trophic levels of nodes
#'
#' Determines the trophic level of each node in an acyclic directed graph.
#'
#' @param edges A data.frame with columns \code{from} and \code{to}.
#'
#' @return A named numeric vector giving the trophic level of each node.
#'
#' @keywords internal
compute_levels <- function(edges) {
  nodes <- unique(c(edges$from, edges$to))
  level <- setNames(rep(NA, length(nodes)), nodes)
  indeg <- table(factor(edges$to, levels = nodes))
  level[names(indeg[indeg == 0])] <- 1

  changed <- TRUE
  while (changed) {
    changed <- FALSE

    for (n in nodes) {
      if (is.na(level[n])) {
        preds <- edges$from[edges$to == n]
        if (all(!is.na(level[preds]))) {
          level[n] <- max(level[preds]) + 1
          changed <- TRUE
        }
      }
    }
  }
  level[order(level)]
}


#' Subset method for trophic_tbl
#'
#' Ensures that any subsetting or modification preserves the validity
#' of the trophic table.
#'
#' @param x A trophic_tbl object.
#' @param ... Additional arguments passed to the base method.
#'
#' @return A validated trophic_tbl object.
#'
#' @export
`[.trophic_tbl` <- function(x, ...) {
  res <- NextMethod()
  validate_trophic_tbl(res)
}

#' Plot a trophic table
#'
#' Creates a simple graphical representation of a trophic network using ggplot2.
#'
#' @param x A \code{trophic_tbl} object.
#' @param ... Additional arguments (not used, for S3 consistency).
#'
#' @details
#' Nodes are positioned according to their trophic level:
#' \itemize{
#'   \item The y-axis represents trophic levels
#'   \item Nodes of the same level are placed on the same horizontal line
#'   \item The x-axis positions are assigned sequentially (0, 1, 2, ...)
#' }
#' Directed links are drawn from lower to higher trophic levels using arrows.
#'
#' @return A ggplot object.
#'
#' @examples
#' net <- trophic() |>
#'   add_link("a", "b") |>
#'   add_link("b", "c")
#'
#' plot(net)
#'
#' @export
plot.trophic_tbl <- function(x, shift=TRUE, ...) {
  stopifnot(inherits(x, "trophic_tbl"))

  levels <- attr(x, "level")
  nodes <- names(levels)

  # Construction d'une table des noeuds avec coordonnées
  node_df <- do.call(rbind, lapply(unique(levels), function(lvl) {
    nds <- nodes[levels == lvl]
    shuff <- ifelse(shift, 1/max(levels)^2 * lvl^2/2, 0)
    data.frame(
      node = nds,
      x = seq_along(nds) - 1 + shuff,
      y = rep(lvl, length(nds)),
      stringsAsFactors = FALSE
    )
  }))

  # Table des arêtes
  edges <- data.frame(
    from = sapply(x$link, `[[`, "from"),
    to   = sapply(x$link, `[[`, "to"),
    stringsAsFactors = FALSE
  )

  # Merge pour récupérer les coordonnées numériques
  edges <- merge(edges, node_df, by.x = "from", by.y = "node")
  edges <- merge(edges, node_df, by.x = "to", by.y = "node", suffixes = c("_from","_to"))

  # Forcer les coordonnées à numeric (par sécurité)
  edges$x_from <- as.numeric(edges$x_from)
  edges$y_from <- as.numeric(edges$y_from)
  edges$x_to   <- as.numeric(edges$x_to)
  edges$y_to   <- as.numeric(edges$y_to)

  node_df$x <- as.numeric(node_df$x)
  node_df$y <- as.numeric(node_df$y)


  ggplot() +
    # arêtes orientées
    geom_segment(
      data = edges,
      aes(x = x_from, y = y_from, xend = x_to, yend = y_to),
      arrow = arrow(length = unit(0.2, "cm"))
    ) +
    # noeuds
    geom_point(
      data = node_df,
      aes(x = x, y = y),
      alpha=0.6, color="grey",
      size = 4
    ) +
    # labels
    geom_text(
      data = node_df,
      aes(x = x, y = y, label = node),
      vjust = -1
    ) +
    scale_y_continuous(
      breaks = sort(unique(levels)),
      limits = c(1,max(levels)+0.5)) +
    labs(
      x = "Node index within trophic level",
      y = "Trophic level",
      title = "Trophic network"
    ) +
    theme_minimal()
}

####################################
#' Get resource layers for a given trophic layer
#'
#' Returns the upstream neighbors (prey/resources) of a given layer in a trophic graph.
#'
#' @param trophic_tbl A \¢ode{trophic_tbl} object
#' @param layer Name of the layer (string) in the spacemodel.
#'
#' @return A character vector of names of layers that are resources for \code{layer}.
#'
#' @export
lower_neighbors <- function(trophic_tbl, layer) {
  stopifnot(inherits(trophic_tbl, "trophic_tbl"))
  # Extract links
  edges <- data.frame(
    from = sapply(trophic_tbl$link, `[[`, "from"),
    to   = sapply(trophic_tbl$link, `[[`, "to"),
    stringsAsFactors = FALSE
  )
  # check if layer is in to
  if (!layer %in% edges$to) {
    return(NULL)
  }
  edges_layer <- edges[edges$to == layer, ]
  unlist(edges_layer$from)
}

#' Normalize weights of a trophic table
#'
#' Adds a new column \code{normalized_weight} to a \code{trophic_tbl} object so that,
#' for each target node (\code{to}), the sum of incoming weights equals 1.
#'
#' @param tbl A \code{trophic_tbl} object.
#'
#' @details
#' For every unique value in the \code{to} elements of the \code{link} column,
#' the function divides each corresponding weight by the total weight of all links
#' pointing to that same node.
#'
#' Nodes with no incoming links are left unchanged.
#'
#' @return A \code{trophic_tbl} object with an additional column
#' \code{normalized_weight}.
#'
#' @examples
#' net <- trophic() |>
#'   add_link("a", "b", weight = 2) |>
#'   add_link("c", "b", weight = 3)
#'
#' net_norm <- normalize_weights(net)
#'
#' @export
normalize_weights <- function(tbl) {

  stopifnot(inherits(tbl, "trophic_tbl"))

  # Extract from/to vectors
  edges <- data.frame(
    from = sapply(tbl$link, `[[`, "from"),
    to   = sapply(tbl$link, `[[`, "to"),
    weight = tbl$weight,
    stringsAsFactors = FALSE
  )

  # Compute sum of weights per target node
  sums <- tapply(edges$weight, edges$to, sum)

  # Normalize
  normalized <- edges$weight / sums[edges$to]

  # Add the new column
  tbl$normalized_weight <- as.numeric(normalized)

  tbl
}

