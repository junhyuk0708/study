CHAPTER3

##3.2 이산변수

#3.2.2 자주 사용되는 이산분포

#1 p.56
hist( x<-rbinom(1000,10,0.3))
mean(x)
var(x)

#2 p.56
par(mfrow=c(1,2))
n=10; p=0.3
hist( x<-rbinom(1000,n,p), prob=T, xlab="n=10", ylim=c(0,0.3), main="")
curve(dnorm(x,n*p,sqrt(n*p*(1-p))), from=0, to=8, type="l" ,add=T)

n=50; p=0.3
hist( x<-rbinom(1000,n,p), prob=T, xlab="n=50", ylim=c(0,0.14), main="")
mu=n*p; sd=sqrt(n*p*(1-p))
curve(dnorm(x,mu,sd), from=mu-3*sd, to=mu+3*sd, type="l" ,add=T)

#3 p.58
par(mfrow=c(1,2))
x<- c(0:12)
plot( x,dpois(x,2), xlab="x", ylab="p(x|theta=2)", type="h" )
lines( x,dnorm(x,2, sqrt(2)), lty=1 )

x<-c(0:100)
plot( x,dpois(x,20), xlab="x", ylab="p(x|theta=20)", type="h" )
lines( x,dnorm(x,20, sqrt(20)), lty=1 )

rx <- rpois(3000,2)
mean.x <- mean(rx)
var.x <- var(rx)

#4 p.59
x<-seq(0:100)
plot(x,dnbinom(x,10,0.2),type="h",xlab="y",ylab="f(y|r=10,theta=0.2)")
title("Density function of NB(10,0.2)")


##3.3 연속변수

#3.3.2 자주 사용되는 연속 분포들

#1 p.64
par(mfrow=c(1,1))
x=seq(0,12, length=100)

plot(x,dgamma(x,1,1),type="l")
lines(x,dgamma(x,2,1),lty=2)
lines(x,dgamma(x,5,1),lty=3)
lines(x,dgamma(x,10,1),lty=4)

legend(5,0.6, c("alpha=1, beta=1","alpha=2, beta=1","alpha=5,beta=1","alpha=10, beta=1"),lty=1:4 )

#2 p.66
x=seq(0,15,length=100)
a=5; b=10
fx=b**a/gamma(a)* x**(-a-1)*exp(-b/x)

rgam=rgamma(1000,a,b)
rIG=1/rgam

hist(rIG, prob=T, xlab="x", ylim=c(0,0.5), main="")
lines(x,fx)

##3.4 다변량 분포

#1 p.69
install.packages('multinomRob')

par(mfrow=c(1,2))

library(multinomRob)

n=10; theta=c(0.5, 0.2, 0.3)
nsim=1000
Rmultinom=matrix(0,nsim,3)

for (i in 1:nsim) Rmultinom[i,]=rmultinomial(n,theta)

hist(Rmultinom[,1], prob=T, xlab="X1", main="")
hist(Rmultinom[,2], prob=T, xlab="X2", main="")

#2 p.71
#Dirichelet generator
rDirich<-function(n,p,a){
	x=matrix(0,n,p)
	for(i in 1:p) x[,i]=rgamma(n,a[i])
	x=x/apply(x,1,sum)
}

n=5000
theta=c(3,4,2)
x=rDirich(n,3,theta)
apply(x,2,mean) # true mean of x = (0.333, 0.444,0.222)

plot(x[,1],x[,2], xlab="x1", ylab="x2")
