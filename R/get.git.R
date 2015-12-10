library(httr)
library(AnaliTools)
library(magrittr)


#' get.git
#' @description Function returns the raw-text code in a specific GitHub file.
#' @param path - The raw.githubusercontent.com path to the file. Should be similar
#' to the path used by api.github.com
#' @return Function returns the code as a character string
#' @export
#' @examples get.git("robertzk/3chessengine/master/.gitignore")
#' get.git("robertzk/3chessengine/master/README.md")
#' get.git("robertzk/3chessengine/master/bower.json")
get.git <- function(
  path,
  two.fac=0,
  user = getOption("GITHUB_USER"),
  pwd = getOption("GITHUB_TOKEN")
){

  #Main API call.
  thing <- GET(
     paste("https://raw.githubusercontent.com/",path,sep="")
    ,authenticate(user, pwd)
    ,add_headers("X-GitHub-OTP" = two.fac)
  )

  #Sanitize the output. Needs to be text and should not be some kind of vector.
  content(thing) %>%
    as.character %||%
    "" %>%
    paste(collapse = " ")
}


