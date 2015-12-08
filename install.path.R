
install.path <- function(path){
  
  for(r in list.repos(path)){
    tryCatch(
      {install_github(r)}
      ,error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
    )
  }

}

#install.path('users/kirillseva')
