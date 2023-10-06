CHAPTER10

##10.1 깁스 표본기법

#1 p.229
M=3000 ; m=500
mu0=10; sigsq0=25; a=0.5; b=1
x=c(10,13, 15, 11, 9, 18, 20, 17, 23,21)
n=length(x)
xbar=mean(x); var.x=var(x)
THETA=matrix(nrow=M ,ncol=2)

#initial value of sigsq
sigsq<-var.x

#####simulation##########################
for(nsim in 1:M) {

#generate mu
condpost.mu=(sigsq/n * mu0 +sigsq0*xbar)/(sigsq/n +sigsq0)
condpost.var=1/(1/sigsq0 +n/sigsq)
mu=rnorm(1, condpost.mu,sqrt(condpost.var))

#generate sigsq 
condpost.a=a+n/2
condpost.b =b+1/2*((n-1)*var.x +n *(xbar -mu)^2)
sigsq =1/rgamma(1,condpost.a , condpost.b)

#save
THETA[nsim, ] = c(mu,sigsq)
}
#######End Simulation ###############################

par(mfrow=c(1,3))

plot(THETA[1:5, ], type="n",xlab=expression(mu),ylab=expression(sigma^2))
lines(THETA[1:5, ],lty=2)
for(i in 1:5) text(THETA[i,1 ],THETA[i,2],i)

plot(THETA[1:15, ], type="n",xlab=expression(mu),ylab=expression(sigma^2))
lines(THETA[1:15, ], lty=2)
for(i in 1:15) text(THETA[i,1 ],THETA[i,2],i)

plot(THETA[1:100, ], type="n",xlab=expression(mu),ylab=expression(sigma^2))
lines(THETA[1:100, ], lty=2)
for(i in 1:100) text(THETA[i,1 ],THETA[i,2],i)

par(mfrow=c(2,1))
plot(THETA[,1], type="l", xlab= "mu" , ylab="")
plot(THETA[,2], type="l", xlab= "sigsq", ylab="")

par(mfrow=c(1,1))
plot(THETA[m:M, 1 ],THETA[m:M, 2 ],xlab=expression(mu), ylab=expression(sigma^2), main="")

par(mfrow=c(1,2))
plot(density(THETA[m:M,1]),xlab=expression(mu),ylab="marginalposterior", main="")
plot(density(THETA[m:M,2]),xlab=expression(sigma^2),ylab="marginalposterior", main="")

#2 p.235
Y=c(14,1,1,1,5); n=22; a=c(0.25,0.25,0.25,0.25)
b=c(0.125,0,0,0.375); c=0.5; alpha=c(1,1,1);
m=1000; N=3000

THETA= matrix(0,m+N,3); X=c(1:9)*0

#Dirichelet generator
rDirich=function(n,p,a){
	x=matrix(0,n,p)
	for(i in 1:p) x[,i]=rgamma(n,a[i])
	x=x/apply(x,1,sum)
}

#initialize
theta= 0.5; eta=0.5; THETA[1,]= c(theta,eta, 1-theta-eta)
X[1]= 0.5*Y[1]; X[3]= 0.5*Y[2]; X[5]= 0.5*Y[3]
X[7]=0.5*Y[4]; X[9]=Y[5]

###################simulate#############################
for(i in 2:(m+N)){
	p=3
	aa=c(X[1]+X[3]+alpha[1],X[5]+X[7]+alpha[2],X[9]+alpha[3])
	THETA[i,]=rDirich(1,p,aa)
	theta=THETA[i,1]; eta=THETA[i,2]
	X[1]=rbinom(1,Y[1], a[1]*theta/(a[1]*theta + b[1]))
	X[3]=rbinom(1,Y[2], a[2]*theta/(a[2]*theta + b[2]))
	X[5]=rbinom(1,Y[3], a[3]*eta/(a[3]*eta + b[3]))
	X[7]=rbinom(1,Y[4], a[4]*eta/(a[4]*eta + b[4]))
}
#################End simulation ##########################

theta.sim=THETA[(m+1):(m+N),1]; eta.sim=THETA[(m+1):(m+N),2]

par(mfrow=c(1,1))
plot(theta.sim,eta.sim, xlab=expression(theta),ylab=expression(eta))

par(mfrow=c(1,2))
plot(density(theta.sim), type="l",xlab=expression(theta),ylab="posterior", main=" ")
plot(density(eta.sim), type="l", xlab=expression(eta), main="")

