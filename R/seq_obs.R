#' Get observed frequencies of transitions
#' @param x a character sequence
#' @param l the lag
#' @return Table of observed frequencies
#' @examples
#' seq_obs("abacbabacbabcababacbcbcbabaaabababbababcb")
#' seq_obs("abacbabacbabcababacbcbcbabaaabababbababcb", 2)
#' @section Further details:
#' Generates a table of frequencies of observed transitions
#' at specified lag
#' @export
#'


seq_obs<-function(x, l=1){

dat<-t(data.frame(uncollapse(x)))
Markovmatrix <- function(X,l){table(X[,-c((ncol(X)-l+1):ncol(X))] , c(X[,-c(1:l)]))}
obs<-Markovmatrix(as.matrix(dat), l)

return(obs)
}
