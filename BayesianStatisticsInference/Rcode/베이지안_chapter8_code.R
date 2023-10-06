CHAPTER9

##8.4 베이지안 양면 검정

#1 p.200
a=2; b=1 ; n=50; x=102 ; theta0=2
B01= dpois(x,n*theta0)/dnbinom(x,a,b/(n+b))
B01




