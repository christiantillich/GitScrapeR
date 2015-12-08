# Function returns the raw text of a specific GitHub file
get.git <- function(url,two.fac=0){
  
  #Main API call. 
  thing <- GET(
     paste("https://raw.githubusercontent.com/",url,sep="")
    ,authenticate(GITHUB_USER, GITHUB_TOKEN)
    ,add_headers("X-GitHub-OTP" = two.fac) 
  )
  
  #Sanitize the output. Needs to be text and should not be some kind of vector.
  content(thing) %>%
    as.character %||%
    "" %>%
    paste(collapse = " ")
}

# get.git("robertzk/3chessengine/master/.gitignore")
# get.git("robertzk/3chessengine/master/README.md")
# get.git("robertzk/3chessengine/master/bower.json")
