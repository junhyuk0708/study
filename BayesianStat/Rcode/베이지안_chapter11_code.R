CHAPTER11

##11.5 JAGS 실행

#1 p.254
install.packages("rjags")
library(rjags)
install.packages("coda")
library(coda)

modelString="
model
{
	for(i in 1:n){
		x[i] ~ dnorm( mu, invsigsq) # 데이터의 분포
	}
	mu ~ dnorm(mu0, invsigsq0) # 사전분포
	invsigsq ~ dgamma(a,b)
	sigsq <- 1/invsigsq # 모수의 변환
	mu0<- 10 # 상수값 지정
	invsigsq0 <- 1/25
	a<- 0.5
	b<- 1
}
"
writeLines(modelString, "model_ex11_1.txt")

dataList=list( n=10, x=c(10,13,15,11,9,18,20,17,23,21))

initsList=list(mu=10 , invsig=0.04)
initsList=function(){
	resampledX=sample(x, replace=T)
	muInit=sum(resampledX)/length(resampledX)
	invsigsqInit= (1/sd(resampledX))^2*0.999+0.01
	return( list( mu=muInit, invsigsq=invsigsqInit))
}

jagsModel=jags.model( file="model_ex11_1.txt", data=dataList,inits=initsList,n.chains=3, n.adapt=500)
update(jagsModel, n.iter=500)

codaSamples=coda.samples(jagsModel, variable.names=c("mu", "sigsq"),n.iter=5000)

coda::traceplot( codaSamples[,"mu"] , main="" , ylab="mu" )
acf(codaSamples[,"mu"][[1]],plot=T, main="")

coda::traceplot( codaSamples[,"sigsq"] , main="" , ylab="sigsq" )
acf(codaSamples[,"sigsq"][[1]],plot=T, main="")

MuSamples=as.matrix(codaSamples[,"mu"])
SigSamples=as.matrix(codaSamples[,"sigsq"])

par(mfrow=c(1,2))
plot(density(MuSamples), xlab=expression(mu), ylab="posterior density",main="")
plot(density(SigSamples), xlab=expression(sigma^2), ylab="posterior density",main="")

AcceptRate=1-rejectionRate(codaSamples)
AcceptRate