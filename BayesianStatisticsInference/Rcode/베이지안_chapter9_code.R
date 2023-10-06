CHAPTER9

##9.1 정보 사전분포와 무정보 사전분포

#1 p.209
sig=1 ; n=5; xbar=-0.1
se=sig/sqrt(n)

theta=seq(0, 1.5, length=100)
post= dnorm( theta, xbar, se) / pnorm( xbar/se)
theta.neg=seq(-2,0, length=100)
post.neg= dnorm( theta.neg, xbar, se) / pnorm( xbar/se)
theta.all=c(theta.neg,theta)
post.all=c(post.neg,post)

plot(theta.all, post.all,type="n",xlab="theta",ylab="posterior")
lines(theta,post,lty=1)
lines(theta.neg,post.neg,lty=2)

##### 95% HPD #####
c= qnorm( pnorm(xbar/se)*0.95 + pnorm(-xbar/se) ) *se + xbar
abline( v=c(0,c), lty=3)
text( 0.4, 0.25, "95% HPD")




