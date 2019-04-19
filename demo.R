############linear model ###############
##basic settings:
library(MASS) #use the function mvrnorn to generate multivariate normal vector
n=600 #number of observations
G=3 #number of groups
p=4 #dimension of X
p1=p2=p3=1/3 #simplest case with equal priors
sd1=sd2=sd3=1 #standard deviation of the residuals from normal distribution
b1=c(1,0,0,0)
b2=c(0,1,1,0)
b3=c(0,0,0,1)
a1=-5;a2=0;a3=5
mu=rep(0,p) #mean vector of x
Sig <- matrix(rep(0,p*p),nrow = p)
for (i in 1:(p-1)) {
  for (j in (i+1):p) {
    Sig[i,j] <- 0.5^(j-i)
  }
}
Sig <- Sig+t(Sig)+diag(1,nrow = p) #generate the covariance matrix of x (with entries 0.5^|i-j|)
x <- mvrnorm(n,mu,Sig)
group <- sample(1:3,size = n,replace = T,prob = c(p1,p2,p3))
D <- data.frame(x,group)
x1 <- as.matrix(subset(D,group==1)[,-(p+1)])
x2 <- as.matrix(subset(D,group==2)[,-(p+1)])
x3 <- as.matrix(subset(D,group==3)[,-(p+1)])
######generate Y's#########
y1 <- a1+x1%*%b1+rnorm(dim(x1)[1],mean = 0,sd=sd1)
y2 <- a2+x2%*%b2+rnorm(dim(x2)[1],mean = 0,sd=sd2)
y3 <- a3+x3%*%b3+rnorm(dim(x3)[1],mean = 0,sd=sd3)
frame <- rbind(data.frame(x1,y=y1,group=1),data.frame(x2,y=y2,group=2),data.frame(x3,y=y3,group=3))
hist(frame$y)


#############我的版本####################
#生成混合高斯分布随机向量
#tag=TRUE表示生成的向量带组标签
library("MASS")
r_Gaussian_mixed <- function(n=100,mu,sigma,weight,tag=FALSE){ 
  
  #默认值
  if (missing(mu)|missing(sigma)|missing(weight)){
    mu <- matrix(0,2,2)
    sigma <- array(rep(diag(1,2,2),2),dim = c(2,2,2))
    weight <- c(0.5,0.5)
  }
  
  r <- runif(n,0,1)
  c.weight <- cumsum(weight)
  if (tag){
    r_G_m <- matrix(0,ncol = nrow(as.matrix(mu))+1,nrow = n)
    i <- 1
    for (x in r){
      order <- sum(c.weight<x)+1
      r_G_m[i,] <- c(mvrnorm(1,mu[,order],sigma[,,order]),order)
      i=i+1
    }
  }
  else{
    r_G_m <- matrix(0,ncol = nrow(as.matrix(mu)),nrow = n)
    i <- 1
    for (x in r){
      order <- sum(c.weight<x)+1
      r_G_m[i,] <- mvrnorm(1,mu[,order],sigma[,,order])
      i=i+1
    }
  }
  return(r_G_m)
}

#第一步，通过高斯混合分布生成y
mu <- matrix(c(rep(1,3),0,rep(2,3),0,rep(3,3),0),nrow = 4)
s <- matrix(0,nrow = 3,ncol = 3)
for (i in 1:3){
  for (j in 1:3){
    sigma[i,j] <- 0.5^abs(i-j)
  }
}
library(Matrix)
s <- array(rep(as.matrix(bdiag(sigma,1)),3),dim = c(4,4,3))
X_e <- r_Gaussian_mixed(n = 1000,mu = mu,sigma = s,weight = rep(1/3,3),tag = TRUE)#随机向量
B <- matrix(c(1,1,1,2,2,2,3,3,3),nrow = 3,ncol = 3)#回归系数的斜率项，每一列对应一个混合成分
A <- c(1,2,3)#回归系数的截距项，每一列对应一个混合成分
f <- function(x){
  n <- length(x)
  class <- x[n]
  y <- A[class]+t(B[,class])%*%x[-1:-2]+x[n-1]
  return(y)
}
y <- apply(X_e,1,f)#生成y的随机数
#############我的版本####################



################classification#################

