

#' list.repos
#' @description Lists all the repos at a given api.github.com path.
#' @param path - A user or org path, e.g. 'users/christiantillich'
#' @return Returns a list of the full paths to any visible repo on the account.
#' @export
#' @examples list.repos('users/kirillseva')
list.repos <- function(path){
  path <- if(grepl('.+/repos$',path)) {path} else{paste(path,'repos',sep="/")}

  show.github.path(path) %>%
    look.for('full_name')
}
