# W4400 HW2
# Mengxi Li
# uni : ml3577


# data are stored in X, one column = one data sample, # of rows = # of samples

# using perceptron to find the classifer
# X: data, one column one data sample, the first row must be ones
# w: weights of data, y: labels of data
# return: (z, classified_labels, objectFunc_value) 
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

# calc the derivative of the percept approximation function
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

  pars = list(z = z, cfy = cfy(X,z), obj = obj(X,w,y,z))

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
# allPars: parameters of all weak learners, a list of list(j,m,theta)
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

# return the error function
errorRate = function(y1, y2, weight=NULL)
{
  if(weight == NULL) return sum( y1 != y2 ) / length(y1)
  else return ( (y1 != y2) * weight ) / sum(weight)
}


# return the error vector  on adaBoost
# X_test: test data
# y_test: labels of test data
# (alpha, allPars): adaBoost parameters
# return: error vector contains errors vs the learning stage
adaBoostError = function(X_test, y_test, alpha, allPars)
{
  error_vec = c()
  for(i in 1:length(alpha)){
    classify_y = agg_class(X_test, alpha[1:i], allPars[1:i])
    error_vec = c(error_vec, errorRate(y_test, classify_y))
  }
  return(error_vec)
}

# return the K-fold cross validation on adaBoost
# data: list(X, y, split), split is a vector showing the data blocks
# adaParsList: a list contains K adaPars, each is the classifer result of
#              returned by adaBoost()
# return: error vector contains errors vs the learning stage
adaBoostFoldError = function(data, adaParsList)
{
  K = length(adaParsList)
  errorMat = matrix()
  for( i in 1:K){
    X_test = _Select_col(data$X, (data$split == i) )
    y_test = subset(data$y, (data$split == i) )
    alpha = adaParsList[i]$alpha
    allPars = adaParsList[i]$allPars
    errorMat = rbind(errorMat, 
                     adaBoostError(X_test, y_test, alpha, allpars))
  }
  return( colMeans(errorMat) )
}

# run K fold adaBoost learn
# data: list(X, y, split), split is a vector of 1,2,...K showing
#        which data belongs to which group
# return: a list contains K adaPars, each for one split
adaBoostFoldLearn = function(data)
{
  result_pars = c()
  K = max(data$split)
# run adaBoost for each split
  for(i in 1:K){
    x_train = _Select_col(data$X, (split != i) )
    y_train = subset(data$y, (split != i) )
    result_pars = c(result_pars, adaBoost(x_train, y_train))
  }
}

# return the seperation of data into K groups
_FoldSplit = function(X,y,K)
{
  split = sample(1:K, length(y), replace=T)
  return list(X=X, y=y, split=split)
}

# select the corresponding columns in X, where vec is TRUE
_Select_col = function(X, vec){
 return( t( subset(t(X), vec) ) )
}

# the main part:
_Main = function()
{
  K = 5
  X = 
  y = 

  data = _FoldSplit(X,y,K)
  adaPars_list = adaBoostFoldLearn(data)
  errors = adaBoostFoldError(data, adaPars_list)
  plot(1:length(errors), errors)
}

#data = load('uspsdata.txt')
#cl   = as.vector(load('uspscl.txt'))
#n_block = 5


