
# kernel ridge regression

krr <- function(data,yName,lamb,kern,newX)
{
   if (!allNumeric(data)) stop("'data' must be all numeric")

   ycol <- which(names(data) == yName)
   data <- as.matrix(data)

   x <- data[,-ycol,drop=FALSE]
   x <- cbind(1,x)
   n <- nrow(x)
   p <- ncol(x)

   newX <- as.matrix(newX)
   if (ncol(newX) == 1) newX <- t(newX)
   newX <- cbind(1,newX)

   # x <- scale(x,center=TRUE,scale=FALSE)
   # xCtr <- attr(x,"scaled:center")
   # newX <- scale(newX,center=xCtr,scale=FALSE)
   y <- data[,ycol,drop=FALSE]

   # predsORR = (newX %*% t(x)) %*% (solve(x%*%t(x) + lamb * diag(n)) # %*% y)

   b <- t(x) %*% solve(x%*%t(x) + lamb * diag(n)) %*% y
   predsORR <- newX %*% b

   # for predsKRR, replace dot products by kernel calls
   part1 <- kernAB(newX,t(x),kern)
   part2 <- kernAB(x,t(x),kern)
   part2 <- solve(part2 + lamb * diag(n)) %*% y
   
   predsKRR <- part1 %*% part2
   return(cbind(predsORR,predsKRR))
}

# finds the matrix product ab, but with the kernel evaluated at each
# entry; optimize this later

kernAB <- function(a,b,k)
{
   ab <- matrix(nrow=nrow(a),ncol=ncol(b))
   for (i in 1:nrow(a)) {
      arow <- a[i,]
      for (j in 1:ncol(b)) {
         ab[i,j] <- k(arow,b[,j])
      }
   }
   ab
}

