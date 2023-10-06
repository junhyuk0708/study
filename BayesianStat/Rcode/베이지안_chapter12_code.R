CHAPTER12

##12.2 계층적 모형에 대한 베이즈 추론

#1 p.272
max.ni=31
data=matrix(scan('http://home.ewha.ac.kr/~msoh/Bayesianbook/mathscore.txt'),ncol=max.ni+2,byrow=T)
ni=data[,2]
K=length(ni)
X=data[,3:(max.ni+2)]

xbar = rep(0,K) ;s = rep(0,K) ; x.min= rep(0,K) ; x.max = rep(0,K)
for (i in 1:K) x.min[i] = min(X[i,1:ni[i]])
for (i in 1:K) x.max[i] = max(X[i,1:ni[i]])
for (i in 1:K) xbar[i] = mean(X[i,1:ni[i]])
for (i in 1:K) s[i] = sd(X[i,1:ni[i]])

# plot Fig 12.2
par(mfrow=c(1,1))
plot(xbar, ylim=c(50,210), type="n", xlab="school", ylab="math score")
for(i in 1:K) for(j in 1:ni[i]) points(i,X[i,j])
for(i in 1:K) lines(c(i,i), c(x.min[i],x.max[i]) )
for(i in 1:K) points(i,xbar[i], pch=19)
xtotal.mean =mean(xbar)
lines(c(-1:100),rep(xtotal.mean,102), lwd=4)

#plot Fig 12.3
par(mfrow=c(1,2))
hist(xbar, xlab="xbar_i", ylab="relative frequency")
hist(s,nclass=10, xlab="s_i", ylab="relative frequency")

#2 p.273
#prior
a=0.5
sigsq0=mean(s**2)
b=a*sigsq0
c=0.5
tausq0=var(xbar)
d=c*tausq0

k0<-1
mu0<-mean(xbar)

##########Start Gibbs ###############

Nwarm=1000; Nsim=5000
THETA=matrix(nrow=Nsim,ncol=K)
MTS=matrix(nrow=Nsim,ncol=3)
theta=c(1:K)

# initial values
mu=mu0
tausq=tausq0
sigsq=sigsq0
theta=xbar

for(ns in 1:(Nwarm+Nsim) ){
	#generate theta
	for(i in 1:K){
		mi=(xbar[i]*ni[i]/sigsq + mu/tausq)/(1/tausq+ni[i]/sigsq)
		vari=1/(1/tausq+ni[i]/sigsq)
		theta[i]=rnorm(1,mi,sqrt(vari))
	}

	#generate sigsq
	alpha=0.5*sum(ni)+a
	sum=0
	for(i in 1:K) for (j in 1: ni[i]){
		sum=sum+(X[i,j]- theta[i])^2
	}
	beta=b+ 0.5*sum
	sigsq=1/rgamma(1,alpha,beta)

	#generate mu
	mu=rnorm(1,(K*mean(theta)+ k0*mu0)/(K+k0),sqrt(tausq/(K+k0)))

	#generate tausq
	alpha=c +0.5*K +0.5
	sum=0
	for(i in 1:K)sum=sum+(theta[i]- mu)^2
	beta=d + 0.5*(k0*(mu-mu0)^2+sum)
	tausq = 1/rgamma(1,alpha,beta)

	#store
	if( ns > Nwarm) {
		THETA[ns-Nwarm,]=theta
		MTS[ns-Nwarm,]=c(mu,sigsq,tausq)
	}
}

######End Gibbs######

#3 p.275
#time sequence plot
par(mfrow=c(1,3))
plot(MTS[,1], ylab="mu")
plot((MTS[,2]), ylab="sigsq")
plot((MTS[,3]), ylab="tausq")

#Auto correlation graph
par(mfrow=c(1,3))
acf(MTS[,1], ylab="ACF mu")
acf(MTS[,2], ylab="ACF sigsq")
acf(MTS[,3], ylab="ACF tausq")

#posterior density function of mu, sigma^2, tau^2
par(mfrow=c(1,3))
plot(density(MTS[,1]),xlab=expression(mu),main="")
abline(v=quantile(MTS[,1],c(0.025,0.5,0.975)), lty=c(3,2,3))

plot(density(MTS[,2]), xlab=expression(sigma^2),main="")
abline(v=quantile(MTS[,2],c(0.025,0.5,0.975)), lty=c(3,2,3))

plot(density(MTS[,3]), xlab=expression(tau^2),main="")
abline(v=quantile(MTS[,3],c(0.025,0.5,0.975)), lty=c(3,2,3))

#inference
mu.hat=mean(MTS[,1])
sigsq.hat=mean(MTS[,2])
tausq.hat=mean(MTS[,3])
theta.hat=apply(THETA,2,mean)

#plot of theta.hat_i
par(mfrow=c(1,1))
theta.grid=seq(mu.hat-7*sqrt(tausq.hat),mu.hat+7*sqrt(tausq.hat),length=100)
hist(theta.hat,prob=T,main="")
lines(theta.grid,dnorm(theta.grid,mu.hat,sqrt(tausq.hat)))

xtotal.mean=sum( ni*xbar)/sum(ni)
par(mfrow=c(1,1))
plot(c(1:K),xbar , ylim=c(100,150), xlab="i")
points(c(1:K),theta.hat, pch=19)
lines(c(1:K), rep(xtotal.mean, K), lw=4)
for(i in 1:K) lines( c(i,i),c( xbar[i], xtotal.mean))

plot(ni,abs(xbar-theta.hat),xlab="n_i",ylab="|xbar_i- theta.hat_i|")

#4 p.281
theta.rank=K+1-rank(theta.hat)
xbar.rank=K+1-rank(xbar)
aa=cbind(ni, round(xbar,1), round(theta.hat,1), xbar.rank, theta.rank)
aa[1:20,]


##12.3 JAGS를 이용한 사후표본추출

#1 p.282
library("rjags")

modelString="
model
{
	for(i in 1:K){
		for (j in 1:ni[i]){
			x[i,j] ~ dnorm( theta[i], invsigsq)
		}
	}
	
	for(i in 1:K){
		theta[i] ~ dnorm( mu, invtausq)
	}

	mu ~ dnorm(mu0, k0*invtausq)
	invsigsq ~ dgamma(a,b)
	invtausq ~ dgamma(c,d)

	sigsq= 1/invsigsq
	tausq= 1/invtausq
}
"
writeLines(modelString, "model_ex12_1.txt")

max.ni=31
data=matrix(scan('http://home.ewha.ac.kr/~msoh/Bayesianbook/mathscore.txt'),ncol=max.ni+2,byrow=T)
ni=data[,2]
K=length(ni)
X=data[,3:(max.ni+2)]

xbar = rep(0,K) ;s = rep(0,K)
for (i in 1:K) xbar[i] = mean(X[i,1:ni[i]])
for (i in 1:K) s[i] = sd(X[i,1:ni[i]])

#prior
a=0.5
sigsq0=mean(s**2)
b=a*sigsq0
c=0.5
tausq0=var(xbar)
d=c*tausq0

k0<-1
mu0<-mean(xbar)

dataList=list(K=K, x=X, ni=ni, a=a, b=b, c=c,d=d, k0=k0, mu0=mu0)
initsList= list( theta=xbar, mu=mu0, invtausq= 1/tausq0,invsigsq=1/sigsq0)

nChains=3

jagsModel=jags.model( file="model_ex12_1.txt", data=dataList,inits=initsList,
n.chains=nChains, n.adapt=100)

update(jagsModel, n.iter=1000)

codaSamples=coda.samples(jagsModel, variable.names=c("theta","mu","sigsq","tausq"),n.iter=30000)

variable.names(codaSamples[[1]])

install.packages("coda")
require(coda)

para=c("mu","sigsq", "tausq")

par(mfrow=c(3,2))
for(i in 1:3){
	traceplot( codaSamples[,para[i]] , main="" , ylab=para[i] )
	acf(codaSamples[,para[i]][[1]],plot=T, main="")
}

MCMCSamples=as.matrix(codaSamples)
par(mfrow=c(1,3))
for(i in 1:3){
	plot( density( MCMCSamples[,i]), main="",xlab=para[i])
	HPD=quantile( MCMCSamples[,i], c(0.025, 0.975))
	abline(v=HPD, lty=2)
}



