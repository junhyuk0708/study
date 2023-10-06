CHAPTER4

##4.2 우도함수

#1 p.90
theta<-seq(0,1,length=200)
ltheta<-120*theta^3*(1-theta)^7

plot(theta, ltheta,type="l")
abline(v=0.3, lty=2)

##4.4 베이지안 구간추정

#1 p.99
theta<-seq(-3,3,length=500)
par(mfrow=c(1,2))
plot(theta, dnorm(theta,0.3,0.5), type="l",ylab="density")
abline(v=qnorm(c(0.049, 0.999), 0.3,0.5) , lty=2)

plot(theta, dnorm(theta,0.3,0.5), type="l",ylab="density")
abline(v=qnorm(c(0.025, 0.975), 0.3,0.5) , lty=2)

#2 p.103
HPDgrid=function(prob,level=0.95){
	prob.sort=sort(prob, decreasing=T)
	M=min( which(cumsum(prob.sort)>= level))
	height=prob.sort[M]
	HPD.index=which( prob >= height)
	HPD.level=sum(prob[HPD.index])
	res=list(index=HPD.index, level=HPD.level)
	return(res)
}

N=1001
theta=seq(-3,3,length=N)

prob=dnorm(theta,0.3, 0.5)
prob=prob/sum(prob)
alpha=0.05
level=1-alpha

HPD=HPDgrid(prob, level)

plot(theta[HPD$index],rep(1,length(HPD$index)))

HPDgrid.hat=c( min(theta[HPD$index]), max(theta[HPD$index]) )
HPDgrid.hat

HPD$level

#3 p.105
HPDsample=function(theta,level=0.95){
	N=length(theta)
	theta.sort=sort(theta)
	M=ceiling(N*level)
	nCI=N-M # number of possbile CIs
	CI.width=rep(0,nCI)
	for( i in 1:nCI)CI.width[i]=theta.sort[i+M]-theta.sort[i]
	index=which.min(CI.width)
	HPD=c(theta.sort[index], theta.sort[index+M])
	return(HPD)
}

N=30000
theta=rnorm(N,0.3, 0.5)
alpha=0.05
level=1-alpha

HPDsample(theta)
