################################### HEADER ###################################

lib <- paste0(path.projects,"/Data_Sci/Github_Scraper")
setwd(lib)
source('show.github.path.R')

################################ END HEADER ##################################


url <- "repos/avantcredit/analytics-partners/issues?state=all"

#Parse each issue
issue.parse <- function(x) {
  data.frame(
    id           = gsub("\\D+","",x$url)
    ,author       = x$user$login
    ,created_at   = x$created_at
    ,url          = x$url
    ,state        = x$state
    ,body         = x$body
    ,closed_at    = coalesce(x$closed_at, "", null.func=is.null)
    ,comments     = x$comments
    ,comments_url = x$comments_url
  ) %>% return
}

issues <- show.github.path(url) %>% 
  lapply(issue.parse) %>% 
  do.call(rbind,.) %>%
  mutate(
     comments_url = as.character(comments_url)
    ,created_at = as.Date(created_at)
    ,closed_at =  as.Date(gsub('^$',NA,as.character(closed_at)))
  )


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

comments <- issues$comments_url %>%
  gsub('https://api.github.com/','',.) %>% 
  sapply(show.github.path, USE.NAMES=F) %>% 
  unlist(recursive=F) %>% 
  lapply(comment.parse) %>%
  do.call(rbind,.) %>% 
  mutate(body = as.character(body))

#If you want to view all the comments a little more neatly:
#for(i in comments$body) print(i)
