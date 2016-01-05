#' install.path
#' @description Installs every repo at a particular user's path, skipping over
#' any repo that is not an R repo.
#' @param path - The api.github.com path to a user account or organization, or
#' anywhere where repos are stored.
#' @return Null. But it sure will print out a ton of diagnostic stuff, so get
#' ready. There's usually a bunch of dependency bullshit in a user's list of repos,
#' so if you get errors on packages that are legit R repositories, just keep
#' running this function until the error list stabilizes
#' @usage install.path('users/robertzk')
#' @export
install.path <- function(path){

  for(r in list.repos(path)){
    tryCatch(
       {install_github(r)}
      ,error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
    )
  }

}

#
