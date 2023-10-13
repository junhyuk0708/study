CHAPTER6

##6.1 ???Æ¼? ?????? ???Äº???

#1 p.145
x1 = rep(c(0, 1, 2, 3, 4, 5, 6), c(7, 14, 13, 8, 4, 2, 2))
x2 = rep(c(0, 1, 2, 3, 4, 5, 6, 7), c(4, 13, 15, 6, 2, 2, 3, 1))

par(mfrow=c(1,2))
plot(table(x1), xlab="x1", ylab="frequency", main="City 1", type="h",xlim=c(0,7))
plot(table(x2), xlab="x2", ylab="frequency", main="City 2", type="h",xlim=c(0,7))

#2 p.146
a =2; b = 1
n1 = length(x1); s1 = sum(x1)
n2 = length(x2); s2 = sum(x2)

postmean.theta1 = (a+s1)/(b+n1)
postmean.theta2 = (a+s2)/(b+n2)

### plot the posterior
par(mfrow=c(1, 1))

theta <- seq(0, 6, length=100)


plot(theta, dgamma(theta, a+s1, b+n1), type="l", xlab="theta",ylab="p(theta | x)")
lines(theta, dgamma(theta, a+s2, b+n2), lty=2)
lines(theta, dgamma(theta, a, b), lty=3)

legend(3.5, 1.5, legend=c(paste("City 1"),
	paste("City 2"), paste("Gamma(2, 1) prior")),
	lty=c(1, 2, 3), bty="n")

#3 p.147
a = 1/2; b = 0

plot(theta, dgamma(theta, a+s1, b+n1), type="l", xlab="theta",ylab="p(theta | x)")
lines(theta, dgamma(theta, a+s2, b+n2), lty=2)
lines(theta, 1/sqrt(theta), lty=3)

legend(3.5, 1.5, legend=c(paste("City 1"),
	paste("City 2"), paste("Jeffrey prior")),
	lty=c(1, 2, 3), bty="n")

##6.2 ????????

#1 p.150
#predictive distribution of X_{n+1}
x1=c(rep(0,7),rep(1,14),rep(2,13),rep(3,8),rep(4,4),rep(5,2),rep(6,2))
x2=c(rep(0,4),rep(1,13),rep(2,15),rep(3,6),rep(4,2),rep(5,2),rep(6,3),rep(7,1) )

a <-2 ; b<- 1
n1 <-length(x1); s1<- sum(x1)
n2 <-length(x2); s2<- sum(x2)
x<- seq(0:10)

par(mfrow=c(1,2))
plot(x,dnbinom(x,size=a+s1,prob=(b+n1)/(b+n1+1)), xlab="x_{n+1}",
ylab="P(x_{n+1}|x_1,...,x_n)", type="h", main="City 1")
plot(x,dnbinom(x,size=a+s2,prob=(b+n2)/(b+n2+1)), xlab="x_{n+1}",
ylab="P(x_{n+1}|x_1,...,x_n)",type="h", main="City 2")

##6.3 ????Ä®?? ?Ù»?

#1 p.153
a<-2 ; b<-1
n1<- 50; s1<- 102 ; n2<- 46;   s2<- 104 ; 
nsim<-30000
theta1.sim = rgamma(nsim,a+s1,b+n1)
theta2.sim = rgamma(nsim,a+s2,b+n2)

eta=theta1.sim- theta2.sim
mean(eta) 
var(eta)

HPD=HPDsample(eta)

par(mfrow=c(1,1))
plot(density(eta), type="l", xlab="theta1-theta2",ylab="posterior density", main="")
abline( v= HPD, lty=2)
text(mean(eta),0.3, "95% HPD interval" )

##6.4 ????Ä®?? ????À» ?Ì¿??? ???????? ??Á¤

#1 p.156
a=2; b=1
n = 10; sx = 17
xnew<-c(0:9)

par(mfrow=c(1,3))

# true predictive distribution
pred.xnew <- dnbinom(xnew, size=a+sx, prob=(b+n)/(b+n+1))
plot(xnew,pred.xnew, type="h",ylab="predictive density",xlim=c(0,9),ylim=c(0,0.3),main="true")

#empirical predictive distribution: when f(x_n+1|theta) is #available
nsim<-1000
theta.sim<-rgamma(nsim, a+sx, b+n)
hat.pred.1<-c(1:10)*0
for(i in 1:10){
	hat.pred.1[i] =mean(theta.sim^xnew[i]*exp(-theta.sim)/gamma(xnew[i]+1) )
}

plot(xnew, hat.pred.1, type="h", ylab="predictive density",xlim=c(0,9), ylim=c(0,0.3), main="empirical_1")

xnew.sim <- rpois(nsim, theta.sim)   # f(xnew|theta)
       # marginal of xnew ~ f(xnew|x_1,.., x_n)
       # Thus, use the sample of xnew for predictive dist.

plot (table (xnew.sim )/ nsim, type="h", ylab = "predictive density",xlim=c(0,9), ylim=c(0,0.3), main="empirical_2")
