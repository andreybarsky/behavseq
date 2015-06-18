#' Get transition probabilities of transitions
#' @param x a character sequence
#' @param l the lag
#' @return Table of transition probabilities
#' @examples
#' seq_trans("abacbabacbabcababacbcbcbabaaabababbababcb")
#' seq_trans("abacbabacbabcababacbcbcbabaaabababbababcb", 2)
#' @section Further details:
#' Generates a table of transition probabilities
#' at specified lag
#' @export
#'


seq_trans<-function(x, l=1){

  dat<-t(data.frame(uncollapse(x)))

  Markovmatrix1 <- function(X,l, prob=T){
    tt <- table(X[,-c((ncol(X)-l+1):ncol(X))] , c(X[,-c(1:l)]))
    if(prob) tt <- tt / rowSums(tt)
    return(tt)
  }

  transm<-Markovmatrix1(as.matrix(dat),prob=T, l)

  return(transm)
}




