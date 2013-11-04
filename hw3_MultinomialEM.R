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
  # normalize <- function(x) if(sum(abs(x)) > 0) x / sqrt(sum(x^2)) else x
  normalize <- function(x) if(sum(x) != 0) {x / sum(x)} else x
  T_dK <- replicate(K, normalize(runif(d,min=0.01, max=10))) #d by K matrix
  print(T_dK)
  C <- rep(1, K)  # K vector
  A <- replicate(K, rep(0, n))

  while(sigma >= tau) 
  {
    T_dK[T_dK <= 0] = 1e-20
    Phi = exp( H %*% log(T_dK) ) # n by K matrix
    pre_A = A
    A = (C * Phi) / (rowSums(C * Phi)) # n by K matrix
    A[abs(A) <= 0] = 1e-20
    C = colMeans(A) # K vector
    B = t(H) %*% A # d by K matrix
    # T_dK = apply(B, 2, function(x) if(sum(x)>0) {x / sum(x)} else x ) 
                                      # normalize it by column, d by K
    T_dK = apply(B, 2, normalize) 
    message("dim of T_dK: ", dim(T_dK), " T_dK: ", min(T_dK), "-",  max(T_dK))
    sigma = max(colSums(abs(A - pre_A)))
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
        + 0.01)
  K <- 4
  tau <- 0.01
  m <- MultinomialEM(H,K,tau)
  image(matrix(m, 200) )
}
