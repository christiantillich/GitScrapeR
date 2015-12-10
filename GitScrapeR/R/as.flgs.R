as.flgs <-
function(df,x){
  #' This function converts a single column into a matrix of binary
  #' flags, one for each unique value the column can take. The columns
  #' can then be readily joined with the original data frame using cbind.
  #' Be careful with this function at present, as you can easily flood 
  #' memory by choosing columns with large numbers of unique values.   
  
  #If any of the columns are not already factors, make them factors
  df <- sapply(df, as.factor)
  
  #Convert the chosen factor column into a matrix of binary flags.
  M <- as.data.frame(model.matrix(~df[,x] - 1))
  names(M) <- gsub('df\\[, x\\]',paste(x,"_",sep=""), names(M))
  
  #Pick the column with the greatest sum. Drop it. This is your reference.
  N <- names(colSums(M)[order(colSums(M), decreasing=T)[1]])
  M[,N] <- NULL
  
  return(M)
}
