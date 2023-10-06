CHAPTER 1

##1.3 자료의 형태

#1.3.1 벡터 

#1 p.6
x=c(1,2,3,4)
x

xx=c(1:4)  ; xx

2*x

2+x

y=c(2,4,5,8)
x+y

#2 p.6
x1<- seq(10)
x1

x2<-seq(1,10)
x2

x3<-seq(1:10)
x3

x4<-seq(from=1, to=10, by=2)
x4

x5<-seq(1,10,2)
x5

x6<-c(x1,x3)    # x1과 x3를 연결하여 하나의 벡터로 만듦
x6

x7 <- seq(0,1, length=11) # 0과 1 사이 등간격으로 11개의 숫자

#3 p.7
rep(1,5)

rep( c(2,3), 3)

ABC = c( "A", "B", "C")
rep(ABC, 2)

rep(ABC, times=2)

rep(ABC, times=c(4,2,1))

rep(ABC, each=2)

rep(ABC, each=2, times=2) 

#4 p.8
x=c(1, 2, 3, 4)

x[1]

x[1:3]

x[c(-1,-3)]  # x[1] , x[3] 원소는 제거된다.

x[c(F,T,F,T)]  #F 에 해당되는 원소는 제거된다.

#1.3.2 팩터(Factor) 

#1 p.8
x=c("high","med","low","med","high")

xf=factor(x)
xf

as.numeric(xf)

#2 p.9
xf2=factor(x, levels=c("low", "med","high"))
xf2

as.numeric(xf2)

#1.3.3 행렬과 어레이

#1 p.9
A=matrix(c(1:9),3)
A

B=matrix(c(1:9), 3, byrow=T)
B

eng<-c(60,72,57,90,95,72)     #English score
math<-c(75, 80, 65, 90, 74, 91)    #Math score

C=cbind(eng,math) 
C

D=rbind(eng, math)
D

#2 p.10
J=matrix(1,3,3)
J    # 원소가 모두 1인 행렬
 
I=diag(3)   # 대각원소가 1인 3차원 대각행렬
A=J*0.3+0.7*I
A      # 대각원소는 1, 나머지는 0.3인 행렬    
 
Ainv=solve(A)   # Ainv= A의 역행렬
Ainv%*%A   # 3차원 identity matrix,  반올림 등의 수치적 오차로 정확한 0값이 안 나올  수 있음  


b=c(1,0.5,1.5)
c=A%*%b     #행렬과 행렬의 스칼라 곱
c

det(A)    # A 행렬의 행렬값(determinant)

B=matrix( c(1:9),3,3)
B
 
C=A+B    # 행렬의 합
C

t(C)      # 행렬의 전치(transpose)

#3 p.12
x=array(1:24, dim=c(3,4,2))
x

#4 p.13
C[4,1] #C=cbind(eng, math)

C[1:5,1]

C[,2]

x=array(1:24, dim=c(3,4,2))

x[,,1] #x는 최근에 만든 3x4x2 어레이

x[3,2,1]

x[,2,1]

#1.3.4 리스트와 데이터 프레임

#1 p.14
MyList= list( Eng=eng, Math=math, Name="final")
MyList

MyList$Eng

MyList[[1]]

MyList$Eng[2]

MyList[[1]][2]

#2 p.14
data(iris)
head(iris)

names(iris)

#3 p.15
YourList=list( a=c(1:2), b=matrix(c(1:9),3,3), c="example")
YourList

##1.4 자료의 입력과 출력

#1.4.1 화면에서 입력

#1 p.16
x<-scan(text="
1 2 3
4 5 6
")

x<-scan(text="
1 2 3 4 5
6 7 8 9 10
")
x

x<-matrix(scan(text="
1 2 3 4
5 6 7 8
9 10 11 12
") , nrow=3, ncol=4)   
x

string.data<-scan(text="Junhee Seungjin Seunghyuk Gil Dong", what="character")
string.data

#1.4.2 외부 파일 입력

#1 p.17
data<-scan('c:/data/chem.txt')

data<-scan('http://home.ewha.ac.kr/~msoh/Bayesianbook/chem.txt')
chem<- matrix(data, ncol=3, byrow=T)
# 위의 두 명령어를 합하면 

chem<- matrix(scan('c:/data/chem.txt'),ncol=3, byrow=T)
chem

#2 p.18
score<-read.table('c:/data/score.txt', header=T)
score
score$ID
score$Total

stotal<-sort(score$Total, decreasing=T)
stotal[1:10]

score[score$Total > 85,]

#3 p.21
write(chem, file='c:/data/chem2.txt', ncol=ncol(chem), append=F, sep="")

#4 p.22
write.csv(score, file="score.csv", row.names=F, quote=F)

##1.5 자료의 통계처리

#1 p.22
mean.Total <- mean(score$Total)
mean.Total

var.Total <- var(score$Total)
var.Total

length.Total<-length(score$Total)
length.Total

summary(score$Total)
   
t.test(score$mid, score$final)

##1.6 그림 그리기

#1.6.1 산점도와 히스토그램

#1 p.24
x<-rnorm(100)   # 표준정규난수 100개 생성
plot(x)     # 그림 1.1
hist(x)     # 그림 1.2

#1.6.2 선 그리기

#1 p.25
theta<-seq(0,1,length=100)
ftheta<-dbeta(theta,3,9) #density function of Beta(3,9) at theta

plot(theta,ftheta,type="l",xlab="theta",ylab="f(theta)") # 95% interval of Beta(3,9) distribution
abline(v=qbeta(c(0.025, 0.975),3,9)) # 2.5th and 97.5th percentile of Beta(3,9)

qbeta(c(0.025, 0.975),3,9) #qbeta(): percentile of Beta dist

#1.6.3 겹쳐 그리기

#1 p.26
plot(theta,ftheta,type="l",xlab="theta",ylab="f(theta)")

ftheta2<-dbeta(theta,5,5)
lines(theta,ftheta2,lty=2) # dotted line

text(0.8,0.6,"Beta(5,5)")
text(0.3,3.0,"Beta(3,9)")

title("Probability density function of Beta distribution")

#2 p.27
x<-rnorm(100)
hist(x, prob=T, xlab=expression(theta))

y<-seq(-3,3,length=100)
lines(y,dnorm(y))  # dnorm(y)= density of N(0,1) at y

#1.6.4 다중 그림 그리기

#1 p.27
y<-c(0:11)
fy<-dpois(y,2) # Poisson(2) density at y
yy<-c(0:100)
fyy<-dpois(yy,20) # Poisson(20) density at yy

par(mfrow=c(1,2))   # par: plot에서의 option 지정, 다중 그림의 줄수는 1, 
		    #열수는 2  

plot(y,fy,type="h") # plot의 type은 히스토그램 형태로 
plot(yy,fyy,type="h")

##1.7 사용자정의 함수

#1 p.29
####### 사용자정의 함수: 다변량정규난수 생성  ##########
rmvnorm<-function(n, mu,Sig){
	p=length(mu)
	R=chol(Sig)
	z=matrix(rnorm(n*p),n,p)
	tt=z%*%R + matrix( mu, n, p, byrow=T)
	return(tt)
}
###############################

####### 사용자정의 함수: 다변량정규분포의 확률밀도함수  ##########
dmvnorm<-function(y,mu,Sig){
	f=exp( -(length(mu)/2)*log(2*pi) -.5*log(det(Sig)) -.5*
		t(y-mu)%*%solve(Sig)%*%(y-mu) )
	return(f)
}
###############################

##1.8 3차원 그림 그리기

#1 p.30
mu<- c(0,0)
Sigma<-matrix(c(1, 0.7, 0.7, 1),ncol=2,nrow=2,byrow=T)
Ysim<-rmvnorm(1000,mu,Sigma)

p<-50
x<-seq(-3,3,length=p)
y<-x
fxy<-matrix(0,p,p)

for(i in 1:p){
  for(j in 1:p){
    xy<-c(x[i],y[j])
    fxy[i,j]<-dmvnorm(xy,mu,Sigma)
  }
}

z<-seq(0.02,0.22,length=6)

# 그림 1.7
contour(x,y,fxy,levels=z) # set levels of the contour plot
points(Ysim)
image(x,y,fxy) # 그림 1.8
persp(x,y,fxy, theta=45) # 그림 1.9

##1.9 도움말과 패키지 이용

#1 p.32
library(MCMCpack) 

help(rinvgamma)