#' code.db
#' @description Recursive function that looks through a repo for all potential
#' scripts, and returns them as a table, including several relevant properties,
#' but more importantly the raw text. The objective is to have all repo contents
#' as a data frame, so the user can analyze it like any other data set.
#' @param repo.path - The api.github.com path to a single repo.
#' @param .tail - I honestly don't remember my reasoning here. It's just the
#' last end of the url. I can't see why the user would want to change it, but
#' YOLO.
#' @param verbose - Binary flag useful for troubleshooting. Will print out each
#' file path checked to terminal if true. Lets you know which files are generating
#' errors.
#' @return returns a single data frame listing all the contents of the repo.
#' @usage head(code.db('repos/robertzk/3chessengine',verbose=T))
#' @export
code.db <- function(repo.path,.tail = '/contents',verbose=F){

  #Grab the top level directory for the repo.
  dir <- show.github.path(paste0(repo.path,.tail))

  #Parsing function for an element.
  content.parse <- function(x){

    #Toggle for troubleshooting.
    if(verbose) {message(c(repo.path,'/', x$name))}

    #If element is a file, return a single-row data frame containing the parsed contents.
    if(x$type == "file"){
      y <- data.frame(
         name            = x$name         %>% as.character %||% ""
        ,type            = x$type         %>% as.character %||% "err"
        ,sha             = x$sha          %>% as.character %||% ""
        ,size            = x$size         %>% as.integer   %||% 0
        ,url             = x$url          %>% as.character %||% ""
        ,html_url        = x$html_url     %>% as.character %||% ""
        ,git_url         = x$git_url      %>% as.character %||% ""
        ,download_url    = x$download_url %>% as.character %||% ""
      )

      #Pull the code if it's an eligible file type.
      eligible <- !grepl('\\.png$',x$name) &
        !grepl('\\.jpg$',x$name) &
        !grepl('\\.gif$',x$name)

      if(eligible){
        y$code <-
          x$download_url %>%
          gsub('https://raw.githubusercontent.com/','',.) %>%
          get.git(.)
      }else{y$code <- NA}
    }

    #If element is a directory, recursively call function, returning many-row data frame.
    if(x$type == "dir"){

      y <- x$url %>%
        gsub('https://api.github.com/','', .) %>%
        gsub('\\?ref=master','',.) %>%
        code.db(.,.tail="",verbose=verbose)
    }

    return(y)

  }

  #Apply parsing function to every element in dir, and collapse to single frame.
  lapply(dir, content.parse) %>%
    do.call(rbind,.) %>%
    lapply(function(x) if(class(x) == "factor") {as.character(x)} else{x}) %>%
    as.data.frame(., stringsAsFactors=F) %>%
    return

  #Return data frame.
}

#User inputs target path.
# test <- code.db("repos/robertzk/3chessengine")
# test %>% var.sum
# test %>% head
# test <- code.db('repos/analyitic')

