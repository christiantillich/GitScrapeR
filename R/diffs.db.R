
#' diffs.db
#' @description Creates a single table and a url. The url will direct you to
#' the diff file, but you have to copy it into the browser yourself. (I'd like to
#' get the full file instead, but I'm already over and need to get this out). The
#' table returned is each changed file, some properties of the file, and the
#' text including the changes.
#' 
#' I'm deprecating this function for the time being, because api.github.com
#' puts a limit on how many files you can pull down, and there's no good way 
#' to get the patch without running into that limit. Instead, I'm going to be
#' building a function that'll parse the raw text pulled manually from 
#' github.com/.../compare/...
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

diffs.db <- function(repo.path, sha.start, sha.end = 'master', type = 'unified'){
  # Compose the diff url
  url <- paste0(repo.path,'/compare/',sha.start,'...',sha.end,'?diff=',type)

  # Function to parse the commits. 
  commit.parse <- function(x){
    data.frame(
       sha     = x$sha
      ,date    = as.Date(x$commit$committer$date)
      ,author  = x$commit$committer$name
      ,message = x$commit$message
      ,url     = x$url
    )
  }
  
  #Function to parse the files
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
  
  #Parse the commits into a table. 
  out$commits <- show.github.path(url)$commits %>%
    lapply(commit.parse) %>% 
    do.call(rbind, .) %>%
    as.data.frame %>% 
    defactorize
  
  #Parse all files into another table
  out$files <- out$commits$url  %>%
    gsub('https://api.github.com/','',.,fixed=T) %>%
    sapply(function(x) show.github.path(x)$files) %>% 
    unlist(recursive=F) %>%
    lapply(files.parse) %>%
    do.call(rbind, .) %>% 
    defactorize
  
  #Turn the persisting row names into the commit_sha
  out$files$commit_sha <- row.names(out$files) %>% gsub('.+/','',.)
  out$files$commit_first_seven <- substr(out$files$commit_sha, 1, 7)
  row.names(out$files) <- NULL

  # Add the url, for reference.
  out$diff.url <- show.github.path(url)$diff_url

#   #Add the data frame.
#   out$diff.data <- show.github.path(url)$files %>%
#     lapply(files.parse) %>%
#     do.call(rbind, .) %>%
#     as.data.frame
# 
#   #do.call(rbind) turns almost all text to factors. Turn back to characters.
#   out$diff.data$patch <- as.character(out$diff.data$patch)
#   out$diff.data$sha <- as.character(out$diff.data$sha)
#   out$diff.data$path <- as.character(out$diff.data$path)
#   out$diff.data$status <- as.character(out$diff.data$status)

  return(out)
}


# test <- diffs.db(
#   'repos/avantcredit/analytics-partners'
#   ,'c23004f0297917e3a889046f6e7d1ffdbb2b876d'
#   ,'master'
#   ,'unified'
# )




# #' Alright, here's the problem. On Dec 15th we made a change to a file called
# #' 'screwed_up_loans.R'. This change doesn't appear to be captured by my parser
# #' tool. Need to figure out why. 
# 
# (diffs$filename == 'screwed_up_loans.R') %>% sum
# 
# url <- paste0(
#   'repos/avantcredit/analytics-partners'
#   ,'/compare/'
#   ,'c23004f0297917e3a889046f6e7d1ffdbb2b876d'
#   ,'...'
#   ,'master'
#   ,'?diff=unified'
# )
# files <- show.github.path(url)$files
# 
# #I think I'm doing this all wrong. There are 58 commits since the Jan 1. 
# show.github.path(url)$commits %>% length
# 
# #I need to get the sha of the first commit...
# show.github.path(url)$commits[[1]]$sha
# 
# #...and then look up the files in that specific commit
# files <- show.github.path('repos/avantcredit/analytics-partners/commits/13d3d6bdc9645ffe3572b2692d4380ffcb7756e6')
# 
# repo.path <- 'repos/avantcredit/analytics-partners'
# sha.start <- 'c23004f0297917e3a889046f6e7d1ffdbb2b876d'
# sha.end <- 'master'
# type <- 'unified'



#' There's still another problem. var.sum on the output only shows 1400 files. 
#' Apparently, there are ~7000 files. So there's still a lot missing. 
