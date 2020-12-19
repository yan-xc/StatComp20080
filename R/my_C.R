#' @title positive mean
#' @name positive mean
#' @description Calculate the mean value of positive.
#' @param X the random sample 
#' @param x critical value
#' @return mean value of positive
#' @importFrom stats optimize quantile
#' @examples
#' \dontrun{
#' X=rnorm(1e3)
#' x=1
#' positive.mean=positive.mean(X,x)
#' }
#' @export
positive.mean=function(X,x){
  n=length(X)
  a=numeric(n)
  for (i in 1:n) {
    if(X[i]-x>=0)
      a[i]=X[i]-x
    else
      a[i]=0
  }
  return(mean(a))
}

#' @title CVaR
#' @name CVaR
#' @description A risk measure to calculate CVaR of loss.
#' @param X the random sample of loss
#' @param alpha confidence level
#' @return CVaR of loss
#' @examples
#' \dontrun{
#' X=rnorm(1e3,5e-4,1e-3)
#' alpha=.05
#' CVaR=CVaR(X,alpha)
#' }
#' @export
CVaR=function(X,alpha){
  f=function(x)
    x+1/(1-alpha)*positive.mean(X,x)
   CVaR=optimize(f,lower = quantile(X,1-alpha),upper = max(X))
   return(CVaR)
}


