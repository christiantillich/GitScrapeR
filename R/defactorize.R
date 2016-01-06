

#' defactorize
#'
#' @param df - A data frame. 
#'
#' @return A data frame where all columns of type factor are replaced with 
#' character columns instead. 
#' @examples
defactorize <- function(df){
  c <- df %>% 
    var.sum %>% 
    filter(type == 'factor') %>% 
    select(var) %>% 
    unlist %>%
    as.character 
  
  df[,c] <- lapply(df[,c], as.character)
  return(df)
}