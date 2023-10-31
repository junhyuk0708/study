###chap2
##1번
post.prob <- function(prior){
  post <- 0.98*prior/(0.98*prior+0.001*(1-prior))
  return(post)
}
#1-1
post1 <- post.prob(0.002) #0.6626099

#1-2
post2 <- post.prob(post1) #0.9994807


##4번
#4-1
#P(BB|M) = 0.5, P(GG|M) = 0.5, P(GB|M) = 0

#4-2
#P(BB|D) = 1/3, P(GG|D) = 1/3, P(GB|D) = 1/3

#4-3
#P(GG) = P(GG,M)+P(GG,D)
#      = P(GG|M)*P(M)+P(GG|D)*P(D)
#      = 1/2*P(M)+1/3*P(D)
#      = 1/2*P(M)+1/3*(1-P(M)) = 1/3 + (1/2*P(M) - 1/3*P(M))
#      = 1/3 + 1/6*P(M)   