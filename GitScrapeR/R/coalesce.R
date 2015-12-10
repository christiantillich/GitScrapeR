coalesce <-
function(..., null.func=is.na) {
  Reduce(function(x, y) {
    i <- which(null.func(x))
    x[i] <- y[i]
    x},
    list(...))
}
