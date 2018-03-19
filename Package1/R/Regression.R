##################################
## Function 1: QR decomposition ##
##################################
#' Perform QR decomposition on the matrix A.
#'
#' @param matrix a matrix of n x m
#' @return A list with Q.transpose and R where Q is an orthogonal n x n matrix and R is an upper triangular n x m matrix
#' @seealso \code{\link{nchar}} which this function wraps
#' @export
#' @examples
#' myQR(A)

myQR <- function(A){

  ## Perform QR decomposition on the matrix A
  ## Input:
  ## A, an n x m matrix

  ########################
  ## FILL IN CODE BELOW ##
  ########################
  n <- dim(A)[1]
  m <- dim(A)[2]
  R <- A
  Q <- diag(rep(1,n))

  for (k in 1:(m-1))
  {
    x = array(0,c(n,1))
    x[k:n,1] = R[k:n,k]
    s = -1 * sign(x[k,1])
    v = x
    v[k] = x[k] - s*norm(x, type="F")
    u = v / norm(v, type="F")

    R = R - 2 * (u %*%(t(u) %*% R))
    Q = Q - 2 * (u %*% (t(u) %*% Q))
  }


  ## Function should output a list with Q.transpose and R
  ## Q is an orthogonal n x n matrix
  ## R is an upper triangular n x m matrix
  ## Q and R satisfy the equation: A = Q %*% R
  return(list("Q" = t(Q), "R" = R))

}

###############################################
## Function 2: Linear regression based on QR ##
###############################################
#' Perform the linear regression of Y on X.
#'
#' @param matrix X is an n x p matrix of explanatory variables
#' @param matrix Y is an n dimensional vector of responses
#' @return The least squares solution vector
#' @seealso \code{\link{nchar}} which this function wraps
#' @export
#' @examples
#' myLinearModel(X, Y)
myLinearModel <- function(X, Y){

  ## Perform the linear regression of Y on X
  ## Input:
  ## X is an n x p matrix of explanatory variables
  ## Y is an n dimensional vector of responses
  ## Do NOT simulate data in this function. n and p
  ## should be determined by X.
  ## Use myQR inside of this function

  ########################
  ## FILL IN CODE BELOW ##
  ########################
  n <- nrow(X)
  p <- ncol(X)

  Z <- cbind(rep(1,n),X,Y)
  L = myQR(Z)
  R1 = L$R[1:(p+1), 1:(p+1)]
  Y1 = L$R[1:(p+1), p+2]
  beta_ls = solve(R1) %*% Y1

  ## Function returns the 1 x (p + 1) vector beta_ls,
  ## the least squares solution vector
  return(beta_ls)

}

##################################
## Function 3: PCA based on QR  ##
##################################
#' Perform PCA on the matrix A.
#'
#' @param matrix A, a square matrix
#' @param integer numIter is the number of iterations
#' @return A list with D and V where D is a vector of eigenvalues of A and V is the matrix of eigenvectors of A
#' @seealso \code{\link{nchar}} which this function wraps
#' @export
#' @examples
#' myEigen_QR(A, 100)
myEigen_QR <- function(A, numIter = 1000){

  ## Perform PCA on matrix A using your QR function, myQRC.
  ## Input:
  ## A: Square matrix
  ## numIter: Number of iterations

  ########################
  ## FILL IN CODE BELOW ##
  ########################
  r <- nrow(A)
  c <- ncol(A)
  V = matrix(rnorm(r*r), nrow=r)
  for(i in 1:numIter)
  {
    L = myQR(V)
    V = A %*% L$Q
  }
  L1 = myQR(V)
  Q = L1$Q
  R = L1$R

  ## Function should output a list with D and V
  ## D is a vector of eigenvalues of A
  ## V is the matrix of eigenvectors of A (in the
  ## same order as the eigenvalues in D.)
  return(list("D" = diag(R), "V" = Q))

}
###################################################################
## Function 4: Linear regression based on QR (without intercept) ##
###################################################################

myLM <- function(X, Y){

  ## Perform the linear regression of Y on X
  ## Input:
  ## X is an n x p matrix of explanatory variables
  ## Y is an n dimensional vector of responses
  ## Use myQR (or myQRC) inside of this function

  ########################
  ## FILL IN CODE BELOW ##
  ########################
  n <- nrow(X)
  p <- ncol(X)

  Z <- cbind(X,Y)
  L = myQR(Z)
  R1 = L$R[1:p, 1:p]
  Y1 = L$R[1:p, p+1]
  beta_ls = solve(R1) %*% Y1


  ## Function returns the least squares solution vector
  return(beta_ls)

}

######################################
## Function 5: Logistic regression  ##
######################################

## Expit/sigmoid function
expit <- function(x){
  1 / (1 + exp(-x))
}

#' Perform the linear regression of Y on X.
#'
#' @param matrix X is an n x p matrix of explanatory variables
#' @param matrix Y is an n dimensional vector of responses
#' @return The logistic regression solution vector
#' @seealso \code{\link{nchar}} which this function wraps
#' @export
#' @examples
#' myLogistic(X, Y)
myLogistic <- function(X, Y){

  ## Perform the logistic regression of Y on X
  ## Input:
  ## X is an n x p matrix of explanatory variables
  ## Y is an n dimensional vector of binary responses
  ## Use myLM (or myLMC) inside of this function

  ########################
  ## FILL IN CODE BELOW ##
  ########################
  n <- nrow(X)
  p <- ncol(X)

  beta <- matrix(rep(0, p), nrow = p)
  epsilon <- 1e-6
  repeat
  {
    eta <- X%*%beta
    pr <- expit(eta)
    w <- pr*(1-pr)
    Z <- eta + (Y-pr)/w
    sw <- sqrt(w)
    mw <- matrix(sw, n, p)
    X1 <- mw*X
    Y1 <- sw*Z
    beta_new <- myLM(X1, Y1)
    err <- sum(abs(beta_new-beta))
    beta <- beta_new
    if (err<epsilon)
      break
  }


  ## Function returns the logistic regression solution vector
  beta

}
