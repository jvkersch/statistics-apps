create_plotgrid <- function(xs, ys) {
  # Note the convention for x and y here: Plotly expects the x-values to increase
  # across the columns, and the y-values across the rows.
  
  X <- outer(rep(1, length(ys)), xs)
  Y <- outer(ys, rep(1, length(xs)))
  
  list(X=X, Y=Y)
}