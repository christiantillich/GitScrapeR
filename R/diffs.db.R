
#' diffs.db
#' @description Creates a single table and a url. The url will direct you to
#' the diff file, but you have to copy it into the browser yourself. (I'd like to
#' get the full file instead, but I'm already over and need to get this out). The
#' table returned is each changed file, some properties of the file, and the
#' text including the changes.
#' @param repo.path - The path to the repo
#' @param sha.start - The full sha key for the commit you wish to start on.
#' @param sha.end - The full sha key for the commit you wish to end on. Defaults
#' to master.
#' @param type - 'unified' or 'split'. Entering any other option here will break
#' the url, so don't do that.
#' @return diff.url - The path to the full change file. Just put it in your
#' browser.
#' @return diff.data - A table with the file name, properties, and the column
#' 'patch', which is the full text of the change. Use this when you want to
#' select subsets of changes for review - e.g. excluding readmes and .png files.
#' @usage
#' df <- diffs.db(
#'   'repos/robertzk/3chessengine'
#'   ,'6591146d25ccb08eae904607f59ac2207e7b7397'
#'   ,'master'
#'   ,'unified'
#'  )
#' df$diff.url
#' head(df$diff.data , 5)
#' @export
diffs.db <- function(repo.path, sha.start, sha.end = 'master', type = 'unified'){
  # Compose the diff url
  url <- paste0(repo.path,'/compare/',sha.start,'...',sha.end,'?diff=',type)

  # File parsing function.
  files.parse <- function(x) {
    code_url <- x$raw_url %>%
      gsub('raw/','',.) %>%
      gsub('.+github.com/','',.)

    data.frame(
       sha               = x$sha %||% NA
      ,path              = gsub('[^/]+$','',x$filename)
      ,filename          = gsub('.*/','',x$filename)
      ,previous_filename = x$previous_filename %||% NA
      ,status            = x$status
      ,additions         = x$additions
      ,deletions         = x$deletions
      ,changes           = x$changes
      ,patch             =
        if(x$status == 'added'){
          x$patch %||% get.git(code_url) %||% NA
        }else(
          x$patch %||% NA
        )
    )
  }

  # set up the container to return.
  out <- list()

  # Add the url, for reference.
  out$diff.url <- show.github.path(url)$diff_url

  #Add the data frame.
  out$diff.data <- show.github.path(url)$files %>%
    lapply(files.parse) %>%
    do.call(rbind, .) %>%
    as.data.frame

  #do.call(rbind) turns almost all text to factors. Turn back to characters.
  out$diff.data$patch <- as.character(out$diff.data$patch)
  out$diff.data$sha <- as.character(out$diff.data$sha)
  out$diff.data$path <- as.character(out$diff.data$path)
  out$diff.data$status <- as.character(out$diff.data$status)

  return(out)
}


# test <- diffs.db(
#   'repos/avantcredit/analytics-partners'
#   ,'c23004f0297917e3a889046f6e7d1ffdbb2b876d'
#   ,'master'
#   ,'unified'
# )




#' File #4 is long. The github API is excluding it, likely because of its size.
#' Do we really want to go and grab it? We can, it doesn't seem to take up
#' much memory, but it's really running R performance into the ground. Do we
#' need it?
