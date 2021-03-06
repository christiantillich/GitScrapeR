#' patch.db
#' @description This function takes in the file generated from a 
#' https://github.com/...repo info.../compare/...  url. 
#' @param file.path - full path to the patch file that needs to be parsed.  
#' @return data - returns a data frame that contains each patch diff as a 
#' separate row entry, along with file name, path, and type info. 
#' @export
patch.db <- function(file.path){
  file <- readLines(file.path) %>% paste(collapse = '\\n')
  
  df <- file %>% 
    strsplit('\\ndiff --git ',fixed=T) %>% 
    unlist(recursive = F) %>% 
    c %>% 
    data.frame(code = .) 
  
  df$code <- as.character(df$code)
  
  files.parse <- function(x){
    files <- gsub('\\\\n.+','',x) %>% 
      strsplit(' ') %>% 
      unlist(use.names=F) 
  }
  
  filepaths <- sapply(df$code, files.parse, USE.NAMES=F)
  filepaths <- filepaths[2:length(filepaths)] %>% do.call(rbind,.) %>% as.data.frame
  colnames(filepaths) <- c('old_file_path','new_file_path')
  
  filepaths$old_name <- gsub('.+/','',filepaths$old_file_path)
  filepaths$new_name <- gsub('.+/','',filepaths$new_file_path)
  filepaths$old_file_path <- gsub('[^/]+$','',filepaths$old_file_path)
  filepaths$new_file_path <- gsub('[^/]+$','',filepaths$new_file_path)
  filepaths$type <- gsub('.+\\.','',filepaths$new_name)
  
  data <- cbind(filepaths, code = df[2:nrow(df),])
  data$code <- paste('\\ndiff --git',as.character(data$code))
  return(data)
}


