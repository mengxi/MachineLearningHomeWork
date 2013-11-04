# Homework 3 of Statistical Machine Learning (W4400)
# Author : Mengxi Li
# Time   : 2013/11/3


# return the hard assignment of multinomial EM algorithm
# args:
#   H : n by d matrix, each row = one sample
#   K : number of clusters
#   tau: stopping threshold
MultinomialEM <- function(H,K,tau)
{
  n <- nrow(H)
  d <- ncol(H)
  sigma <- 1.0
  normalize <- function(x) x / sqrt(sum(x^2))
  T_dK <- replicate(K, normalize(runif(d,min=0.01, max=10))) #d by K matrix
  C <- rep(1, K)  # K vector
  A <- replicate(K, rep(0, n))

  while(sigma >= tau) 
  {
    if(min(T_dK) <= 0 ) T_dK = T_dK + 1e-3
    stopifnot(min(T_dK) > 0)
    Phi = exp( H %*% log(T_dK) ) # n by K matrix
    pre_A = A
    A = (C * Phi) / (rowSums(C * Phi)) # n by K matrix
    C = colMeans(A) # K vector
    B = t(H) %*% A # d by K matrix
    T_dK = t( apply(B, 1, function(x) x / sum(x)) ) # normalize it by row, d by K
    message("dim of T_dK: ", dim(T_dK), " T_dK: ", min(T_dK), "-",  max(T_dK))
    sigma = max(colSums(abs(A - pre_A)))
    if(is.na(sigma)) sigma = tau + 1
    print(sigma)
  }
  # m = argmax A for each row of A
  m = apply(A, 1, function(x) which.max(x))
  message("m: ", length(m), " ", min(m), "-", max(m))
  return(m)
}

# the main function
hw3_main <- function()
{
  H <- (matrix( readBin('histograms.bin','double',640000), 40000, 16) 
        + 1)
  K <- 5
  tau <- 0.05
  m <- MultinomialEM(H,K,tau)
  image(matrix(m, 200) )
}
