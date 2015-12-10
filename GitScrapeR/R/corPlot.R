corPlot <-
function(data, all_vars, dv, cutoff=0.70){
  require(corrplot)
  cor_vars = all_vars[sapply(data[,all_vars],class)%in%c('integer','numeric','logical')]
  cor_dv = data.frame(correlation=t(cor(data[,dv],data[,cor_vars])))
  cor_dv = cor_dv[order(abs(cor_dv[,1]),decreasing=T),,drop=F]
  cor_jnk =  cor(data[,row.names(cor_dv)])
  print(cor_dv)
  corrplot(cor_jnk, order="orig", method="circle", tl.pos="lt", type="upper",        
           tl.col="black", tl.cex=0.6, tl.srt=90, 
           addCoef.col="black", addCoefasPercent = TRUE,
           p.mat = 1-abs(cor_jnk), sig.level=1-cutoff, insig = "blank")
}
