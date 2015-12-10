name.search <-
function(x, top=""){
  for(n in names(x)){
    print(paste(top,"$",n,sep=""))
    #print(names(x[[n]]))
    if(!is.null(names(x[[n]]))){
      name.search(x[[n]], top=paste("$",n,sep=""))
    }
  }
}
