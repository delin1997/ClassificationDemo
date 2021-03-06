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


################classification#################

