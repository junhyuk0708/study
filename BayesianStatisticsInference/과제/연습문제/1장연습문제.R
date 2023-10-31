#Q3
x <- c(12,13,9,4,4,7,10,3,8,3,5,9,14,10,3,6,6,8,9)
y <- c(97,90,89,85,63,75,80,55,78,42,63,78,88,92,82,72,45,72,80)

#(1)
plot(x,y)

#(2)
one <- rep(1,19)
X <- cbind(as.matrix(one),as.matrix(x))
b0 <- (solve(t(X)%*%X)%*%t(X)%*%y)[1]
b1 <- (solve(t(X)%*%X)%*%t(X)%*%y)[2]

#(3)
plot(x,y)
abline(b0,b1)

#Q4
tel <- c('A','A','B','C','B','A','A','A','B','B','C','B','C','A','C','B','A','C','C','B','A','A','B','A','C','B','A','B','B','A')
barplot(table(tel))

#Q5
beta <- seq(0,1,0.01)
plot(beta,dbeta(beta,2,7),type = 'l')

#Q6
x <- seq(-3,3,0.01)
plot(x,dt(x,4),type='l',col='red')
points(x,dnorm(x,0,1),type='l',col='blue')

#Q7
matrix(1:35,nr=3,nc=7,byrow = T)[3,6]

#Q9
x <- seq(-3,3,0.1)
y <- x
f <- function(x,y){2*x^2+3*y^2}
contour(x,y,outer(x,y,f))
