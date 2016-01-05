#' search.repo
#' @description Allows regular-expression search through a single repository.
#' @param path - The api.github.com path to a user or organization
#' @param regex - The regular expression to use when searching through the code.
#' @param name.regex - The regular expression to use when searching for text in
#' the name. Most common use is filetypes, e.g. name.regex = "\\.R$"
#' @param verbose - Boolean. True if you want search.repo to print the full paths
#' of each file in the directory, as it's being scanned. Very useful for
#' troubleshooting
#' @return Function returns a data.frame object containing the name of the file
#' and a 50-character-long snippet of text where the expression is found.
#' @export
search.repo <- function(path, regex=".", name.regex='\\..+',verbose=F){


  #Build the code.db from the path
  message(paste("Scanning",path,"/...  Please be patient!"))
  files <- code.db(path,verbose=verbose)
  message("Scan Complete!")

  #Return each file where the code matches the regex pattern.

  files %>%
    filter(
       grepl(paste0(name.regex,'$'),name,ignore.case=T)
      ,grepl(regex,code,ignore.case=T)
    ) %>%
    mutate(
       loc = sapply(code, function(x) regexpr(regex, x, ignore.case=T)[1], USE.NAMES=F)
      ,code = paste0("..." ,substring(code, try(loc - 25,T), try(loc + 25,T)),"...")
    ) %>%
    select(name,code)
}

#' search.repos
#' @description Regex search over multiple repos.
#' @param paths - A list of repos
#' @param regex - The regular expression to use when searching through the code.
#' @param name.regex - The regular expression to use when searching for text in
#' the name. Most common use is filetypes, e.g. name.regex = "\\.R$"
#' @param verbose - Boolean. True if you want search.repo to print the full paths
#' of each file in the directory, as it's being scanned. Very useful for
#' troubleshooting
#' @return Function returns a data.frame object containing the name of the file
#' and a 50-character-long snippet of text where the expression is found.
#' @usage
#' paths <- c('repos/robertzk/3chessengine','repos/robertzk/testthatsomemore')
#' search.repos(paths, regex = 'Map', '\\.R$',verbose=T)
#' @export
search.repos <- function(paths, regex=".", name.regex='\\..+',verbose=F){
  lapply(paths, function(x) search.repo(x, regex, name.regex,verbose=verbose)) %>%
    do.call(rbind, .) %>%
    return
}
