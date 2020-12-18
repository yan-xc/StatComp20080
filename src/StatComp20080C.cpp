#include <Rcpp.h>
#include<math.h>
using namespace Rcpp;

//' @title A Metropolis sampler using Rcpp
//' @name Metropolis_c
//' @description A Metropolis sampler using Rcpp.
//' @param N the number of samples
//' @param x0 initial value
//' @param sigma variance of samples
//' @return a random sample of size \code{N}
//' @importFrom Rcpp evalCpp
//' @examples
//' \dontrun{
//' N=1e3
//' sigma=2
//' x0=25
//' rw=Metropolis_c(N,x0,sigma)
//' }
//' @export
// [[Rcpp::export]]
NumericVector Metropolis_c(int N,double x0,double sigma) {
  NumericVector x(N);
  x[0]=x0;
  NumericVector u = as<NumericVector>(runif(N));
  for (int i=1;i<N;i++) {
    double y=as<double>(rnorm(1,x[i-1],sigma));
    if (u[i]<=(exp(-abs(y))/(exp(-abs(x[i-1])))))
      x[i]=y ;
    else {
      x[i]=x[i-1];
    } 
  }
  return(x);
}