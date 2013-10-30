# W4400 HW2
# Mengxi Li
# uni : ml3577


# data are stored in X, one column = one data sample, # of rows = # of samples

# using perceptron to find the classifer
# X: data, one column one data sample, the first row must be ones
# return: the final z
Perceptron = function(X, w, y)
{
  d = nrow(X)
  n = ncol(X)

  z = rep(1,d)
  z = z / (z %*% z)
  deri = rep(1,d)
  max = 1000

# the classifer
  cfy = function(X, z){
    return sign( as.vector(z %*% X) )
  }

# the result of the objective function
  obj = function(X, w, y, z){
    ob_y = cfy(X, z)
    error_vec = (ob_y != y)
    return( (w %*% error_vec)/sum(w) )
  }

# calc the derivative of the object function
  derivative = function(X, w, y, z){
    ob_y = cfy(X, z)
    error_vec = (ob_y != y)
    ret = rowSums(w * error_vec * ob_y * X) / sum(w)
    return(ret)
  } 

  while(sum(abs(deri)) > 1e-3 && max > 0){
    deri = derivative(X, w, y, z)
    z = z - deri
    max = max - 1
  }

  pars = list()
  pars$z = z
  pars$cfy = cfy(X,z)
  pars$obj = obj(X,w,y,z)

  return(pars)

}

addone = function(X)
{
  ones = rep(1, ncol(X))
  return(rbind(ones, X))
}


# the weaker learning training routing
# X: data, one column one data sample
# w: vector of weight
# y: vector of  label
# return: parameters specifying the resulting classifier: 
#         pars = (j, theta, m), m=1 or -1
train = function(X, w, y)
{
  d = nrow(X)
  n = ncol(X)
  z_all = c()
  obj_all = c()
  for(j in 1:d){
    X_j = X[j, ]
    per_ret = Perceptron(addone(X_j), w, y)
    z_all = c(z_all, per_ret$z)
    obj_all = c(obj_all, per_set$obj)
  }

  j_min = which.min(obj_all)
  z_min = z_all[ j_min ]
  z1 = z_min[1]; z2 = z_min[2];

  pars = list(j = j_min, m = sign(z2), theta = - z1 / abs(z2) )
  return(pars)
}


# the classification routing evaluating the weak learner on X
# X: data, column = data sample
# pars: a list, parameters for the weak learner
#  pars: (j, m, theta).
# return: label = C(X)
classify = function(X, pars)
{
  n = ncol(X);
  X_j = X[pars$j, ];
  m = sign(pars$m);
  y = rep(-m, n);
  y[ X_j > pars$theta ] = m;
  return(y) 
}


# evaluate the bossting classifer on X
# alpha: vector of voting weights
# allPars: parameters of all weak learners
# return: c_hat = label of X under the boosting classifer
agg_class = function(X, alpha, allPars)
{
  c_all = c()
  for(p in allpars){
    c_all = c(c_all, classify(X, p))
  }
  return( sign( alpha %*% c_all ) )
}


# return the adaboost classifier parameters
# returns: pars = (alpha, allPars)
adaBoost = function(X, y)
{
  n = ncol(X); d = nrow(X);
  B = 100
  w = rep(1/n, n)

  alpha = c()
  allPars = c()

  for(b in 1:B){
    cweak_par = train(X, w, y)
    weak_y = classify(X, cweak_par)
    error = ( (y != weak_y) %*% w )/ sum(w)
    vote_wei = log( (1-error) / error )
    
    alpha = c(alpha, vote_wei)
    allPars = c(allPars, cweak_par)
    
    w = w * exp( vote_wei * (y != weak_y) )

  }

  return( list(alpha = alpha, allPars = allPars) )
}

# the main part:
data = load('uspsdata.txt')
cl   = load('uspscl.txt')
n_block = 5
n_data = ncol(data)
split = sample(1:n_block, n_data, replace=T)

# run adaBoost for each split
for(i in 1:n_block){
  train_set = data[, (split != i)]
}



