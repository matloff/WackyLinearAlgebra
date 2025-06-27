overfit <- function(nreps,n,maxP)
{
    load('YearData.save') 
    nas <- rep(NA,nreps*(maxP-1))
    outdf <- data.frame(p=nas,mape=nas)
    rownum <- 0
    for (i in 1:nreps) {
       idxs <- sample(1:nrow(yr),n)
       trn <- yr[idxs,] 
       tst <- yr[-idxs,]           
       for (p in 2:maxP) {
          rownum <- rownum + 1
          out<-qePolyLin(trn[,1:(p+1)],
             'V1',2,holdout=NULL) 
          preds <- predict(out,tst[,-1]) 
          mape <- mean(abs(preds - tst[,1])) 
          outdf[rownum,1] <- p
          outdf[rownum,2] <- mape
          print(outdf[rownum,])
       }
    }
    outdf  #run through tapply() for the graph
}

