CHAPTER2

##2.3 표본공간의 분할과 베이즈 정리

#1 p.41
aa<-function(pf) 50*pf/(41*pf+9)
x<-c(0:10)*0.1
x

prob<-aa(x)
prob
 
plot(x,x,type="l",xlab="P(F)",ylab="probability")
lines(x,prob,lty=2)
text( 0.6, 0.5, "prior")
text (0.4, 0.85, "posterior")

#2 p.45
post.prob<-function(s,prior){
	if (s==1) post<- 0.95*prior/(0.95*prior+0.7*(1-prior))
	else post<-0.05*prior/(0.05*prior+0.3*(1-prior))
	return(post)
}

prior<-c(1:13)
post<-c(1:12)
data<-c(1,0,1,1,1,1,1,1,1,0,1,0) # 1=s, 0=u
prob<-matrix(0,12,2)

prior[1]<-0.9
for(i in 1:12) {
	post[i]<-post.prob(data[i],prior[i])
	prob[i,]<-c(prior[i], post[i])
	prior[i+1]<-post[i]
}

prob


