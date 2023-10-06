CHAPTER13

##13.3 회귀모형의 베이지안 추론

#13.3.2 표본분산에 비례하는 사전분산을 사용할 경우

#1 p.300
HDL.data=matrix(scan('http://home.ewha.ac.kr/~msoh/Bayesianbook/HDL.txt'),ncol=3, byrow=T)
y=HDL.data[,3]
group=HDL.data[,1]
n=length(y) ; p=6
x=matrix(0,n,p)
for ( i in 1:n ) {
	x[i,group[i]]=1; x[i,group[i]+3]=HDL.data[i,2]
}
xtx=t(x)%*%x
xtx.inv=solve(xtx)
xty=t(x)%*%y
beta.LSE=xtx.inv%*%xty
SSR=t(y)%*%( diag(1,nrow=n) - x %*% xtx.inv %*% t(x) ) %*% y

# Random sample generation
a<-0; b<-0
Nsample=10000 ## num. of samples
sig2=0*c(1:Nsample)
beta=matrix(0, Nsample,p)
for ( iter in 1: Nsample ){
	sig2[iter]=1/rgamma(1, 0.5*n+a, 0.5*SSR+b)
	Sigma.beta=n/(n+1)*sig2[iter]*xtx.inv
	R=t(chol(Sigma.beta))
	beta[iter,]=R%*%(rnorm(p)) + beta.LSE
}

# posterior density of sigma^2, beta
sig2.grid=seq(0,200,length=100)
astar=0.5*n+a ; bstar=as.vector(0.5*SSR+b)
post.sig2=bstar**astar/gamma(astar)*sig2.grid**(-astar-1)*exp(-bstar/sig2.grid)

#Figure 13.1
par(mfrow=c(1,2))
plot(sig2.grid, post.sig2, xlab=expression(sigma^2),ylab="true posterior density", type="l", xlim=c(0,200))
plot(density(sig2), xlab=expression(sigma^2), ylab="estimated posterior density", type="l", main="",xlim=c(0,200))

#Figure 13.2
par(mfrow=c(1,3))
plot(density(beta[,4]), xlab="beta_1", ylab="posterior density",main="")
HPD=HPDsample(beta[,4])
abline(v=HPD , lty=2)

plot(density(beta[,5]), xlab="gamma_1", ylab="posterior density",main="")
HPD=HPDsample(beta[,5])
abline(v=HPD, lty=2)

plot(density(beta[,6]), xlab="delta_1", ylab="posterior density",main="")
HPD=HPDsample(beta[,6])
abline(v=HPD, lty=2)

#2 p.305
#Figure 13.3
par(mfrow=c(1,1))
plot(HDL.data[ ,2], HDL.data[,3], xlab="weight", ylab="HDL cholesterol", type="n")
for(i in 1:n)text(HDL.data[i,2], HDL.data[i,3],HDL.data[i,1])

beta.hat=apply(beta,2,mean)
lines( HDL.data[,2], beta.hat[1]+ beta.hat[4]* HDL.data[,2])
lines( HDL.data[,2], beta.hat[2]+ beta.hat[5]* HDL.data[,2])
lines( HDL.data[,2], beta.hat[3]+ beta.hat[6]* HDL.data[,2])
text( 200,75, "Group I")
text( 210,65, "Group II")
text( 120,68, "Group III")

#Figure 13.4
diff.12=beta[,4]-beta[,5]
diff.13=beta[,4]-beta[,6]
diff.23=beta[,5]-beta[,6]

par(mfrow=c(1,3))
plot(density(diff.12), xlab="beta_1-gamma_1", ylab="posterior density", main="")
HPD=HPDsample(diff.12)
abline(v=HPD , lty=2)

plot(density(diff.13), xlab="beta_1-delta_1", ylab="posterior density", main="")
HPD=HPDsample(diff.13)
abline(v=HPD , lty=2)

plot(density(diff.23), xlab="gamma_1-delta_1", ylab="posterior density", main="")
HPD=HPDsample(diff.23)
abline(v=HPD , lty=2)


##13.4 JAPGS를 활용한 사후표본추출

#1 p.306
library("rjags")

modelString="
model
{
	for(i in 1:length(y)){
		y[i] ~ dnorm( inprod(x[i,], beta[]), invsigsq)
	}

	beta[1:length(beta0)] ~ dmnorm( beta0[], invSig0[,])

	invsigsq ~ dgamma(a,b)
	sigsq= 1/invsigsq
	invSig0= 1/(c*sigsq)*xtx
}
"
writeLines(modelString, "model_ex13_1.txt")

######### read data ###################
HDL.data=matrix(scan('http://home.ewha.ac.kr/~msoh/Bayesianbook/HDL.txt'
),ncol=3, byrow=T)
y=HDL.data[,3]

group=HDL.data[,1]
n=length(y)
p=6
K=p
x=matrix(0,n,p)
for ( i in 1:n ) {
	x[i,group[i]]=1; x[i,group[i]+3]=HDL.data[i,2]
}

xtx=t(x)%*%x
xtx.inv=solve(xtx)
xty=t(x)%*%y
beta.LSE=as.vector(xtx.inv%*%xty)
SSR=t(y)%*%( diag(1,nrow=n) - x %*% xtx.inv %*% t(x) ) %*% y
sigsq.hat=as.numeric(SSR)/(n-p)

#--- prior ----
beta0 = beta.LSE
c=n
a=0.5
b= a* sigsq.hat

dataList=list( x=x, y=y,xtx=xtx, a=a, b=b, c=c, beta0=beta0)
initsList= list( beta=beta0, invsigsq=1/sigsq.hat)

nChains=3
jagsModel=jags.model( file="model_ex13_1.txt", data=dataList,inits=initsList,n.chains=nChains, n.adapt=100)

update(jagsModel, n.iter=1000)

codaSamples=coda.samples(jagsModel, variable.names=c("beta","sigsq"),
n.iter=30000)

variable.names(codaSamples[[1]])

para=c("beta0", "gamma0", "delta0", "beta1", "gamma1", "delta1")

par(mfrow=c(3,2))
for(i in 1:6){
	coda::traceplot( codaSamples[,i] , main="" , ylab=para[i] )
	acf(codaSamples[,i][[1]],plot=T, main="")
}

MCMCSamples=as.matrix(codaSamples)

par(mfrow=c(3,2))
for(i in 1:6) plot( density( MCMCSamples[,i]), main="",xlab=para[i])

par(mfrow=c(1,1))
plot( density( MCMCSamples[,7]), main="",xlab="sigma^2")

acceptRate= 1-rejectionRate(codaSamples)
acceptRate