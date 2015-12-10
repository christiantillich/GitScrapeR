flg.create <-
function(df, name.list){
  #' This function is a wrapper for as.flgs, to apply it to a named list
  #' of columns in a target data frame. This allows you to do batch the
  #' flag conversion of factor variables. As above, be careful with this
  #' function, as it's easy to flood memory by batching more than one
  #' column with large numbers of unique entries. 
  
  #Apply as.flgs to every column name in name.list
  flgs <- lapply(name.list, function(x) as.flgs(df,x))
  
  #flgs returns as a list. Unlist it and force to data frame with names. 
  d <- as.data.frame(unlist(flgs, recursive=F))
  names(d) <- gsub('.+\\.',"", names(d))
  
  #In the original data set, del every column in the named list. 
  df <- df[del(df, name.list)]
  
  #Append the new logical columns to the original data set. 
  df <- cbind(df, d)
  
  #Return the modified data set. 
  return(df)
}
