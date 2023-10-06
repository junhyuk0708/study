CHAPTER5

##5.1 이항비율의 추정

#1 p.116
# theta ~ Beta(a, b)
a<-1 ; b<-1
# X|theta ~ B(n, theta)
n<-40 ; x<-15

# a discretization of the possible theta values
theta = seq(0, 1, length=50)
prior.theta = dbeta(theta, a, b)

# prob of data|theta(likelihood)
likhd.theta = dbinom(x, n, theta)

# joint prob of data & theta
joint.xtheta <- prior.theta*likhd.theta

# posterior of theta 
post.theta <- dbeta(theta, a+x, b+n-x) 

par(mfrow=c(2, 2)) # set up a 2x2 plotting window
plot(theta, prior.theta, type="l", sub="(a) prior: pi(theta)")
abline(v=(a-1)/(a-1+b-1), lty=2) # a vertical line at the mode

plot(theta, likhd.theta, type="l", sub="(b) likelihood: f(x|theta)")
abline(v=x/n,lty=2)

plot(theta, joint.xtheta, type="l", sub="(c) prior x likelihood:pi(theta)x f(x|theta)")
abline(v=(a+x-1)/(a+b+n-2), lty=2)

plot(theta, post.theta, type="l", sub="(d) posterior: pi(theta|x)")
abline(v=(a+x-1)/(a+b+n-2), lty=2)

#2 p.119
par(mfrow=c(1, 1))
plot(theta, post.theta, type="l", col="blue")
lines(theta, prior.theta, col="red", lty=2)

legend(.5, 3, legend=c(paste("beta(",a,", ",b,") prior"),
	paste("post under beta(",a,", ",b,") prior")),
	lty=c(2, 1), col=c("red", "blue"), bty="n")

#3 p.120
# simulation-based inference
theta = rbeta(2000, a+x, b+n-x) # generate posterior samples

hist(theta, prob=T, main="Histogram of theta")
lines(density(theta))

mean.theta = mean(theta)
abline(v=mean.theta, lty=2)

quantile(theta, c(.025, .975)) # simulation-based quantiles

qbeta(c(.025, .975), a+x, b+n-x) # theoretical quantiles

# simulation-based estimates
mean(theta) ; var(theta) 

# theoretical estimates
(a+x)/(a+b+n) ; (a+x)*(b+n-x)/((a+b+n+1)*(a+b+n)^2)

#4 p.122
a=b=1
x=15;n=40
theta=rbeta(10000,a+x,b+n-x)
eta=log(theta/(1-theta))

hist(eta, prob=T, main="Histogram of eta")
lines(density(eta), lty=2)

mean.eta=mean(eta)
mean.eta

var.eta=var(eta)
var.eta

#5 p.126
theta = seq(0, 1, length=50)
a=2; b=10; x=15; n=40; z=5; m=10

prior.theta = dbeta(theta, a, b)
post.theta = dbeta(theta, a+x, b+n-x)
post2.theta = dbeta(theta, a+x+z, b+n-x+m-z)

plot(theta, post.theta, type="l", col="blue")
lines(theta, post2.theta, lty=2, col="black")
lines(theta, prior.theta, lty=3, col="red")

legend(.5, 3, legend=c(paste("beta(",a,", ",b,") prior"),
	paste("beta(",a+x,", ",b+n-x,") posterior"),
	paste("beta(",a+x+z,", ",b+n-x+m-z,")posterior")),
	lty=c(3, 1, 2), col=c("red", "blue", "black"), bty="n")

#6 p.127
par(mfrow=c(1,1))
theta=seq(0,1,length=201)

plot( theta, dbeta(theta,15+2,25+10), ylab="posterior",type="n")
lines( theta, dbeta(theta,16,26),lty=1)
lines( theta, dbeta(theta,15+2,25+10),lty=2)
lines( theta, dbeta(theta,16,27),lty=3)

legend(0.6,3,legend=c("Beta(1,1) prior", "Beta(2,10) prior","Beta(1,2) prior"), lty=c(1,2,3))

##5.2 예측분포

#1 p.130
a <- 1; b <- 1
n <- 40; x <- 15
m <- 10; z <- c(0:10)

pred.z = gamma(m+1)/gamma(z+1)/gamma(m-z+1)*beta(a+z+x,
	b+n-x+m-z)/beta(a+x, b+n-x)

plot(z, pred.z, xlab="z", ylab="probability", type="h")
title("Predictive Distribution, a=1, b=1, n=40, x=15, m=10")

#2 p.132
a=b=1
x=15; n=40
m=10
N=10000
theta=rbeta(N,a+x,b+n-x)
z=rbinom(N,m,theta)

plot(table(z)/N, type="h", xlab="z", ylab="predictive density",main="")

mean(z)
var(z)

##5.3 베이지안 신뢰구간

#1 p.133
a=1; b=1
x=2; n=10
theta=seq(0,1,length=1001)
ftheta=dbeta(theta,a+x, n-x+b)
prob=ftheta/sum(ftheta)

HPD=HPDgrid(prob, 0.95)

HPD.grid=c( min(theta[HPD$index]), max(theta[HPD$index]) )
HPD.grid

#2 p.133
n=10; x=2

CI.exact=binom.confint(x, n, conf.level = 0.95, methods = c("exact"))
CI.exact=c(CI.exact$lower, CI.exact$upper)

CI.exact

#3 p.135
HPD.approx=qbeta(c(0.025, 0.975),a+x, n-x+b)

p=x/n
CI.asympt=c(p-1.96*sqrt(p*(1-p)/n) ,p+1.96*sqrt(p*(1-p)/n) )

HPD.approx
CI.asympt
 
##5.4 두 비율의 비교

#1 p.137
a=b=0.1
n1=18; x1=12 ; n2=10; x2=8

theta1=rbeta(10000,a+x1, b+n1-x1)
theta2=rbeta(10000,a+x2, b+n2-x2)

eta=log(theta1/theta2)
xi=log( (theta1/(1-theta1)) /( theta2/(1-theta2)) )

par(mfrow=c(1,2))

HPD=HPDsample(eta)

plot(density(eta),type="l", xlab="로그비 ",ylab="posterior density", main="")
abline(v=HPD, lty=2)
text(mean(eta),0.3, "95% HPD interval" )

HPD=HPDsample(xi)

plot(density(xi),type="l", xlab="로그 오즈비 ",ylab="posterior density", main="")
abline(v=HPD, lty=2)
text(mean(xi),0.06, "95% HPD interval" )

