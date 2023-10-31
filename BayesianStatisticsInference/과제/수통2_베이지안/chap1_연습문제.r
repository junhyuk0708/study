###chap1
##3번
#3-1 산점도
time <- c(12,13,9,4,4,7,10,3,8,3,5,9,14,10,3,6,6,8,9)
score <- c(97,90,89,85,63,75,80,55,78,42,63,78,88,92,82,72,45,72,80)
dat1 <- data.frame(time, score)
str(dat1)

plot(time, score)

#3-2 선형회귀직선
fit <- lm(score ~ time, data = dat1)
summary(fit)
# y(score) = 51.707 + 3.102*x(time)

#3-3 산점도 + 회귀선
plot(time, score)
abline(fit)

##4번
#히스토그램
ABC = c( "A", "B", "C")
x <- rep(ABC, times=c(12,11,7))
x <- as.factor(x)
barplot(table(x))

##5번
#베타분포 확률밀도함수
theta<-seq(0,1,length=100)
ftheta<-dbeta(theta,2,7) #density function of Beta(2,7) at theta

plot(theta,ftheta,type="l",xlab="theta",ylab="f(theta)") # 95% interval of Beta(2,7) distribution
abline(v=qbeta(c(0.025, 0.975),2,7)) # 2.5th and 97.5th percentile of Beta(2,7)

##6번
#베타분포 확률밀도함수
t <- seq(-3,3,length=100)
ft <-dt(t, df = 4)

plot(t,ft,type="l")

y<-seq(-3,3,length=100)
lines(y,dnorm(y), lty = 2)  # dnorm(y)= density of N(0,1) at 

##7번
#3행 6열의 값
mat <- matrix(1:35, nrow = 3, ncol = 7, byrow = TRUE)

print(mat[3,6]) #20

##9번
#등고선도
x <- seq(-5, 5, length.out = 100)
y <- seq(-5, 5, length.out = 100)

z <- outer(x, y, FUN = function(x, y) 2 * x^2 + 3 * y^2)

contour(x, y, z, xlab = "X", ylab = "Y")


