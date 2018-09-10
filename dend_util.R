# Function to generate edges of dendrogram
hierfly <- function(data, metric="euclidean", method="ward.D2") {
  data <- rescaler(data)
  id <- 1:nrow(data)
  cat <- sapply(data, is.factor)
  h <- hclust(dist(data[,!cat], metric), method)
  data_hc <- data

  data_hc$ORDER <- order(h$order)
  data_hc$HEIGHT <- 0
  data_hc$LEVEL <- 0
  data_hc$POINTS <- 1

  #newr_df <- NULL
  for (i in 1:nrow(h$merge)) {
    newr <- combinerows(data_hc[as.character(-h$merge[i,]),], cat)
    #newr$id <- nrow(data_hc) + i
    newr$HEIGHT <- h$height[i]
    newr$LEVEL <- i
    rownames(newr) <- as.character(-i)

    data_hc <- rbind(data_hc, newr)
  }
  data_hc$id <- 1:nrow(data_hc)
  data_hc$node <- (as.numeric(rownames(data_hc)) < 0) + 0

  vars <- c("ORDER", "POINTS")

  leaves <- subset(data_hc, node == 0)
  nodes <- subset(data_hc, node == 1)

  # < 0 = observation, > 0 = cluster
  edges <- h$merge
  edges[edges > 0] <- edges[edges > 0] + nrow(leaves)
  edges[edges < 0] <- abs(edges[edges < 0])

  return(list(data=data_hc, edges=edges))
}


# Utility functions
combinerows <- function(df, cat) {
  same <- function(x) if (length(unique(x)) == 1) x[1] else x[2]
  points <- df$POINTS

  cont <- as.data.frame(lapply(df[, !cat, drop=FALSE] * points,
                               sum)) / sum(points)
  cat <- as.data.frame(lapply(df[, cat, drop=FALSE], same))

  df <- if (nrow(cont) > 0 && nrow(cat) > 0) {
    cbind(cont, cat)
  } else if (nrow(cont) > 0) {
    cont
  } else {
    cat
  }
  df$POINTS <- sum(points)
  df
}

rescaler <- function(df) {
  is_numeric <- vapply(df, is.numeric, logical(1))
  df[is_numeric] <- lapply(df[is_numeric], rescale01)
  df
}

rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}
