vec.text <-
function(x) {
  x %>% paste(collapse="','") %>% paste("c('",.,"')",sep="")
}
