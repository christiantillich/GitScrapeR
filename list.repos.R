


list.repos <- function(path){
  path <- if(grepl('.+/repos$',path)) {path} else{paste(path,'repos',sep="/")}
  
  show.github.path(path) %>%
    look.for('full_name')
}

#list.repos('users/kirillseva')
