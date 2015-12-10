perf.plot__ <-
function(target, predictor
    , main=""
    ,xlab="x"
    ,ylab="Target"
    ,type="b"
    ,col="red"
)
{
  
  #This function snaps a real value to the closest value in the list v. 
  find <- function(x, v) v[which.min(abs(x - v))]
  
  cat.flg <- length(unique(predictor)) <= 30
  
  #Establish the range to search over for x. 
  if(cat.flg){
    b <- as.data.frame(as.factor(unique(predictor)))
    
    c <- predictor
  }else{
    b <- as.data.frame(hist(predictor, plot=F)$breaks)  
    
    #Use find to snap each record to the bins in x. 
    c <- sapply(predictor, function(x){find(x, b[,1])})
  }
  colnames(b) <- "x"
  
  
  
  #Calculate the count at each bin level. 
  n <- aggregate(target ~ c, FUN=length)
  colnames(n) <- c("x", "n")
  
  #Calculate the target at each bin level. 
  r <- aggregate(target ~ c, FUN = mean)
  colnames(r) <- c("x", "y")
  
  #Summarize everything in a table. 
  t <- merge(merge(b, r, all.x=T), n, all.x=T)
  
  #Replace nulls with 0s for cleanliness
  t$y[is.na(t$y)] <- 0
  t$n[is.na(t$y)] <- 0
  
  #Plot the variable performance and distribution. 
  if(cat.flg){
    plot(t$n
         , xlab="", ylab="",type="h",axes=FALSE,lend = "square",col = "gray"
         ,main=main
         ,lwd = 300 / length(t$n)
    )
    par(new=T)
    plot(t$y
         ,xlab= xlab,ylab= ylab,type = type,col = col,xaxt="n"
         ,ylim=c(min(t$y, 0),max(t$y, 1)) 
    ) 
    axis(1, at = t$x,labels = levels(t$x), las=2)
  } else{
    plot(t$x,t$n
         , xlab="", ylab="",type="h",axes=FALSE,lend = "square",col = "gray"
         ,main=main
         ,lwd = 300 / length(t$n)
    )
    par(new=T)
    plot(t$x,t$y
         ,xlab= xlab,ylab= ylab,type = type,col = col,axes=TRUE
         ,ylim=c(min(t$y, 0),max(t$y, 1))
    ) 
  }
}
