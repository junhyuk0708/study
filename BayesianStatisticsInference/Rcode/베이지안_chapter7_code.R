CHAPTER7

##7.1 분산이 주어진 조건하에서 평균에 대한 추론

#1 p.169
#prior
mu0 = 5.68; sig0 = 5.30; n = 50; xbar = 5.5; s = 4.8

#posterior
c= n/s^2 ; c0=1/sig0^2; w = c/(c+c0)
mu.post = w * xbar + (1-w)* mu0; sig.post = sqrt( 1/(c+c0) )

ta=seq(mu.post-5*sig.post, mu.post+5*sig.post, length=100)

plot(theta, dnorm(theta,mu.post, sig.post), type="l",main="posterior and prior of theta")
lines(theta,dnorm(theta,mu0,sig0),lty=2)
legend(7.5,0.3,legend=c("posterior","prior"),lty=c(1,2),bty="n")

#predictive
mu.new = mu.post; sig.new = sqrt( s^2 + sig.post^2 )
xnew=seq(mu.new - 5* sig.new , mu.new + 5* sig.new, length=100)

plot(xnew, dnorm(xnew, mu.new, sig.new),type="l", main="Predictivedensity of X_new")

##7.2 평균과 분산에 대한 추론

#1 p.175
#prior
mu0=22.5; k0=5; sig0=20.4
a=k0/2; b=(a-1)*sig0*sig0

#data
xbar=27.4 ; n=20 ; s=22.5

#para.
mu.theta.post=(k0* mu0+n*xbar)/(k0+n)
a.post=n/2+a
b.post=1/2*((n-1)*s*s+k0*n/(k0+n)*(xbar-mu0)^2)+b

#true joint posterior
theta.grid=seq(10,45,length=100)
sigsq.grid=seq(100,1200,length=100)

f=function(theta,sigsq,a.post,b.post,mu.theta.post,n,k0){
	library(MCMCpack)
	f= dinvgamma(sigsq,a.post,scale=b.post)*
		dnorm(theta,mu.theta.post,
		sqrt(sigsq/(n+k0)) )
}	

post.joint=outer(theta.grid,sigsq.grid,f,a.post,b.post,mu.theta.post,n,k0)

#Monte Carlo 
Nsim=1000
sigsq.sim=1/rgamma(Nsim,a.post,b.post)
theta.sim=rnorm(Nsim,mu.theta.post,sqrt(sigsq.sim/(k0+n)))

par(mfrow=c(1,1))
plot(theta.sim, sigsq.sim, xlab="theta",ylab="sigma_sq")
contour(theta.grid,sigsq.grid,post.joint,
	level=c(1.e-6,1.e-5,1.e-4,3.e-4),add=T)

#marginal posterior
post.sigsq= dinvgamma(sigsq.grid,a.post, scale=b.post)

post.theta=theta.grid*0
for(j in 1:length(theta.grid) ){
	post.theta[j]= mean( dnorm(theta.grid[j],mu.theta.post,
	sqrt(sigsq.sim/(n+k0)) ))
}

par(mfrow=c(1,2))
plot(theta.grid,post.theta,type="l",xlab="theta",ylab="marginal posterior")
plot(sigsq.grid,post.sigsq,type="l",xlab="sigma_sq",ylab="marginal posterior")
