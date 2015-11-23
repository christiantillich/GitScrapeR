################################### HEADER ###################################

lib <- paste0(path.projects,"/Data_Sci/Github_Scraper")
setwd(lib)
source('show.github.path.R')
source('code.db.R')

################################ END HEADER ##################################

search.repo <- function(url, regex, file.type='\\..+',verbose=F){

  
  #Build the code.db from the url
  message(paste("Scanning",url,"/...  Please be patient!"))
  files <- code.db(url,verbose=verbose)
  message("Scan Complete!")
  
  #Return each file where the code matches the regex pattern. 
  
  files %>%
    filter(
       grepl(paste0(file.type,'$'),name,ignore.case=T)
      ,grepl(regex,code,ignore.case=T)
    ) %>% 
    mutate(
       loc = sapply(code, function(x) regexpr(regex, x, ignore.case=T)[1], USE.NAMES=F)
      ,code = paste0("..." ,substring(code, try(loc - 25,T), try(loc + 25,T)),"...")
    ) %>%
    select(name,code)
}

search.repos <- function(urls, regex, file.type='\\..+',verbose=F){
  lapply(urls, function(x) search.repo(x, regex, file.type,verbose=verbose)) %>%
    do.call(rbind, .) %>%
    return
}

#Some example searches of search.repo
# search.repo('repos/avantcredit/analytics-partners','select','.R')
# search.repo('repos/robertzk/3chessengine','rook')
# search.repo('repos/robertzk/3chessengine',"var [A-Za-z, ]+",file.type=".js")

#Example using search.repos
# repos <- c(
#    "repos/avantcredit/analytics-partners"
#   ,"repos/avantcredit/avant"
# )
# search.repos(repos, 'looker_query',verbose=T)

#Fun null-result example, make sure this does not error. It takes a while. 
# repos = c('repos/avantcredit/avant','repos/avantcredit/avant-basic')
# search.repos(repos, 'looker.yml')
