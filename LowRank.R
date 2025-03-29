
library(imager)
# img <- load.image('prj.png')  
# somehow grayscale() doesn't work on this imgage
img <- load.image('prj1.png')  # already grayscale
imgSVD <- svd(img)
u <- imgSVD$u
v <- imgSVD$v
retainedRank <- 15
newImgMat <- u[,1:retainedRank] %*% diag((imgSVD$d)[1:retainedRank]) %*%
   t(v[,1:retainedRank])
plot(as.cimg(newImgMat))

