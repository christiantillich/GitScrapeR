
library('httr')
library('curl')
library('magrittr')
# library('jsonlite')
# library('httpuv')

#' show.github.path
#' @description Function returns the content of a github path
#' @param path - Not the full path. Just the parts after api.github.com/, e.g.
#' 'user/christiantillich/repos', as a character string.
#' @param two.fac - Added for two-factor authentication. I'm using a PAT, though
#' so I've never actually tested whether this works.
#' @param user - defaults to the global variable GITHUB_USER. Please add this
#' to your global variables in your .RProfile
#' @param pwd - defaults to the global variable GITHUB_TOKEN. Please create a
#' valid token via https://github.com/settings/tokens and add this to your
#' .RProfile. Alternatively, you can just use your password. However, because
#' we're locked into 2-factor authentication here, I haven't tested this.
#' @return This function returns a list object containing everything in the
#' API call. Internally, this is just content(GET(path)), with the domain and
#' authentication credentials all filled out.
#' @export
show.github.path <- function(
  path,
  two.fac=0,
  user = getOption("GITHUB_USER"),
  pwd = getOption("GITHUB_TOKEN")
){
  thing <- httr::GET(
     paste("https://api.github.com/",path,sep="")
    ,authenticate(user,pwd)
    ,add_headers("X-GitHub-OTP" = two.fac)
  )
  content(thing)
}

#Try various paths to make sure it's working.
# show.github.path("user")
# show.github.path("issues")
# show.github.path('repos/avantcredit/analytics-partners/issues')

