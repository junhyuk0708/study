################################################################
#############################P23################################
################################################################
theta <- seq(0, 1, length = 100)
ftheta <- dbeta(theta, 3, 9)
plot(theta, ftheta, type="l", xlab="theta", ylab="f(theta)")
# 95%신뢰구간 
abline(v = qbeta(c(0.025, 0.975),3, 9))

ftheta2 <- dbeta(theta, 5, 5)
plot(theta, ftheta, type="l", xlab="theta", ylab="f(theta)")
lines(theta, ftheta2,lty=2)
text(0.8, 0.6, "Beta(5,5)")
text(0.3, 3.0, "Beta(3,9)")

par(mfrow=c(1,1))
theta <- seq(0, 1, length = 100)
ftheta <- dbeta(theta, 2, 7)
plot(theta, ftheta, type = "l", xlab="theta", ylab="f(theta")
abline(v = qbeta(c(0.025, 0.975), 2, 7))  

################################################################
#############################P24################################
################################################################
x <- rnorm(10000)
hist(x, prob=T, xlab=expression(theta))
y <- seq(-3, 3, length=100)
lines(y, dnorm(y)) # dnorm(y) = density of N(0, 1) at y
################################################################
#############################P25################################
################################################################
y <- c(0:11)
fy <- dpois(y, 2) # Poisson(2) density at y
yy <- c(0:100)
fyy <- dpois(yy, 20)
par(mfrow=c(1,2))
plot(y, fy, type = "h")
plot(yy, fyy, type = "h")
################################################################
#############################P26################################
################################################################
#다변량정규난수 생성
rmvnorm <- function(n, mu, sig){
  p = length(mu)
  R = chol(sig)
  z = matrix(rnorm(n*p), n, p )
  tt = z %>% R + matrix(mu, n, p, byrow =T)
  ruturn(tt)
}
