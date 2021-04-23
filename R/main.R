#' 2D test function for sequential optimization
#'
#' The function is defined on the unit interval [0,1]^2.
#'
#' @param X A matrix
#' @return A vector of function outputs.
#' @examples
#' X <- matrix(c(0.2,0.3,0.1,0.2), ncol=2)
#' testfun(X)
#'
#' @export
testfun <- function(X) {
  testfun_aux(X)
}

testfun_aux <- function(X) {
  # X1 and X2 are on [0,1]; shift to
  # x1 on [-2,2], x2 on [-1,1]
  x1 <- 4*X[ ,1] - 2
  x2 <- 2*X[ ,2] - 1
  -((4 - 2.1*x1^2 + x1^4/3)*x1^2 + x1*x2 + (-4+4*x2^2)*x2^2)
}
