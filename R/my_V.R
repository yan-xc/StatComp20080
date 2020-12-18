#' @title VaR
#' @name VaR
#' @description A risk measure to calculate VaR of loss.
#' @param X the random sample of loss
#' @param alpha confidence level
#' @return VaR of loss
#' @examples
#' \dontrun{
#' X=rnorm(1e3,5e-4,1e-3)
#' alpha=.05
#' VaR=VaR(X,alpha)
#' }
#' @export
VaR=function(X,alpha){
  VaR=quantile(X,1-alpha)
  return(VaR)
}


