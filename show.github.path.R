library('curl')
library('jsonlite')
library('httpuv')
library('httr')
library('dplyr')


# Function returns the content of a github url
show.github.path <- function(url,two.fac=0){
  thing <- GET(
     paste("https://api.github.com/",url,sep="")
    ,authenticate(GITHUB_USER, GITHUB_TOKEN)
    ,add_headers("X-GitHub-OTP" = two.fac) 
  )
  content(thing)
}

#Try various paths to make sure it's working. 
# show.github.path("user")
# show.github.path("issues")
# show.github.path("user/issues")
# show.github.path("user/repos")
# show.github.path("orgs/avantcredit/issues")

