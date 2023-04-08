#' weight function
#' Computes the weights from a given system matrix. The *default* for the length
#' is 100 time steps.
#'
#' @param M system matrix
#' @param len length of the weights
#'
#' @return numeric vector
#' @export
#'
#' @examples weights(M=rbind(c(1.5, -2), c(1, 0)), len=80)
weights <- function(M, len=100){
  # surpress warnings start block
  defaultW <- getOption("warn"); options(warn = -1)
  # import library
  library(expm)
  # surpress warnings end block
  options(warn = defaultW)
  # computing the system matrix
  w <- rep(0, len)
  for(i in 1:len){w[i] <- (M%^%i)[1, 1]}
  return(w)
}

weights(M=rbind(c(1.5, -2), c(1, 0)))
