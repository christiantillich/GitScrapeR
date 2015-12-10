

#' issues.db
#' @description Creates an issues and a comments table from the path to a
#' specific Github repo. Tables are joinable on issues$id = comments$issue_id,
#' and the relationship is 1-many.
#' @param repo.path - The api.github.com path to a single repo.
#' @returns Function returns a list of tables, "issues" and "comments"
#' @export
#' @examples issues.db("repos/avantcredit/analytics-partners")
issues.db <- function(repo.path){

  url <- paste(repo.path, 'issues?state=all', sep="/")

  #Parse each issue
  issue.parse <- function(x) {
    data.frame(
      id           = gsub("\\D+","",x$url)
      ,author       = x$user$login
      ,created_at   = x$created_at
      ,url          = x$url
      ,state        = x$state
      ,body         = x$body
      ,closed_at    = x$closed_at %||% ""
      ,comments     = x$comments
      ,comments_url = x$comments_url
    ) %>% return
  }

  #Parse each comment.
  comment.parse <- function(x) {
    data.frame(
      id           = x$id
      ,issue_id     = gsub("\\D+","",x$issue_url)
      ,author       = x$user$login
      ,created_at   = x$created_at
      ,url          = x$url
      ,issue_url    = x$issue_url
      ,body         = x$body
    )
  }



  out <- list()

  out$issues <- show.github.path(url) %>%
    lapply(issue.parse) %>%
    do.call(rbind,.) %>%
    mutate(
       comments_url = as.character(comments_url)
      ,created_at = as.Date(created_at)
      ,closed_at =  as.Date(gsub('^$',NA,as.character(closed_at)))
    )

  out$comments <- out$issues$comments_url %>%
    gsub('https://api.github.com/','',.) %>%
    sapply(show.github.path, USE.NAMES=F) %>%
    unlist(recursive=F) %>%
    lapply(comment.parse) %>%
    do.call(rbind,.) %>%
    mutate(body = as.character(body))

  return(out)
}



#If you want to view all the comments a little more neatly:
#for(i in comments$body) print(i)
