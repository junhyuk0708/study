#Q1
post.prob <- function(prior){
  post <- 0.98*prior/(0.98*prior+0.001*(1-prior))
  return(post)
}
#(1)
Post1 <- post.prob(0.002)
#(2)
Post2 <- post.prob(Post1)

#Q2

#(1)
#P(BB|M)=0.5
#P(GG|M)=0.5
#P(GB|M)=0

#(2)
#P(BB|D)=1/3
#P(GG|D)=1/3
#P(GB|D)=1/3

#(3)
#P(GG)=P(GG,M)+P(GG,D)
#     =P(GG|M)P(M)+P(GG|D)P(D)
#     =0.5*P(M)+1/3*P(D)
#     =0.5*P(M)+1/3*(1-P(M))
#     =1/3+1/6*P(M)
