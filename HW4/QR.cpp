/*
#########################################################
## Stat 202A - Homework 3
## Author: Devanshi Patel 
## Date : 10/26/17
## Description: This script implements QR decomposition,
## linear regression, and eigen decomposition / PCA 
## based on QR.
#########################################################
 
###########################################################
## INSTRUCTIONS: Please fill in the missing lines of code
## only where specified. Do not change function names, 
## function inputs or outputs. MAKE SURE TO COMMENT OUT ALL 
## OF YOUR EXAMPLES BEFORE SUBMITTING.
##
## Very important: Do not change your working directory
## anywhere inside of your code. If you do, I will be unable 
## to grade your work since R will attempt to change my 
## working directory to one that does not exist.
###########################################################
 
*/ 


# include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;


/* ~~~~~~~~~~~~~~~~~~~~~~~~~ 
 Sign function for later use 
 ~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

// [[Rcpp::export()]]
double signC(double d){
  return d<0?-1:d>0? 1:0;
}



/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
   Problem 1: QR decomposition 
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */  
  

// [[Rcpp::export()]]
List myQRC(const mat A){ 
  
  /*
  Perform QR decomposition on the matrix A
  Input: 
  A, an n x m matrix (mat)

  #############################################
  ## FILL IN THE BODY OF THIS FUNCTION BELOW ##
  #############################################
  
  */ 
  mat R = A;
  int n = R.n_rows;
  int m = R.n_cols;

   mat Q = eye<mat>(n,n);
    
    for (int k = 0; k<(m-1); k++)
    {
      mat x = zeros<mat>(n,1); 
      x.submat(k,0,n-1,0) = R.submat(k,k,n-1,k);
      int s = -1 * signC(x(k,0));
      mat v = x;
      v[k] = x[k] - s*norm(x);
      mat u = v / norm(v);
      
      R = R - 2 * (u *(u.t() * R));
      Q = Q - 2 * (u * (u.t() * Q));
    }
    List output;
  
  
  // Function should output a List 'output', with 
  // Q.transpose and R
  // Q is an orthogonal n x n matrix
  // R is an upper triangular n x m matrix
  // Q and R satisfy the equation: A = Q %*% R
  output["Q"] = Q.t();
  output["R"] = R;
  return(output);
  

}
  
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
   Problem 2: Linear regression using QR 
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  
  
// [[Rcpp::export()]]
mat myLinearRegressionC(const mat X, const mat Y){
    
  /*  
  Perform the linear regression of Y on X
  Input: 
  X is an n x p matrix of explanatory variables
  Y is an n dimensional vector of responses
  Do NOT simulate data in this function. n and p
  should be determined by X.
  Use myQRC inside of this function
  
  #############################################
  ## FILL IN THE BODY OF THIS FUNCTION BELOW ##
  #############################################
  
  */  
  
  mat beta_ls;
  int n = X.n_rows;
  int p = X.n_cols;
    
  mat Z = ones<mat>(n,1);
  Z = join_rows(Z, X);
  Z = join_rows(Z, Y);
  List L = myQRC(Z);
  mat R = L["R"];
  mat R1 = R.submat(0,0,p,p);
  mat Y1 = R.submat(0,(p+1),p,(p+1));
  beta_ls = R1.i() * Y1;
  // Function returns the 'p+1' by '1' matrix 
  // beta_ls of regression coefficient estimates
  return(beta_ls.t());
  
}  

/* ~~~~~~~~~~~~~~~~~~~~~~~~ 
 Problem 3: PCA based on QR 
 ~~~~~~~~~~~~~~~~~~~~~~~~~~ */


// [[Rcpp::export()]]
List myEigen_QRC(const mat A, const int numIter = 1000){
  
  /*  
  
  Perform PCA on matrix A using your QR function, myQRC.
  Input:
  A: Square matrix
  numIter: Number of iterations
   
  #############################################
  ## FILL IN THE BODY OF THIS FUNCTION BELOW ##
  #############################################
   
   */  
  
  
  List output;
  int r = A.n_rows;
  int c = A.n_cols;
  mat V = rnorm(r*r);
  V.reshape(r,c);
    for(int i=0; i<numIter; i++)
    {
      List L = myQRC(V);
      mat Q = L["Q"];
      V = A * Q;
    }
    List L1 = myQRC(V);
    mat Q = L1["Q"];
    mat R = L1["R"];
    vec D = R.diag();
  // Function should output a list with D and V
  // D is a vector of eigenvalues of A
  // V is the matrix of eigenvectors of A (in the 
  // same order as the eigenvalues in D.)
  output["D"] = D.t();
  output["V"] = Q;
  return(output);

}
  
