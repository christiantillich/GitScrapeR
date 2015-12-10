look.for <-
function(list, target){
  sapply(list, function(x) x[[target]])
}
