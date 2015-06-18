#' Get expected frequencies of first order transitions
#' @param x a character sequence
#' @return Table of observed frequencies
#' @examples
#' seq_exp("abacbabacbabcababacbcbcbabaaabababbababcb")
#' @section Further details:
#' Generates the expected frequencies of first order
#' transitions.  Note: this shouldn't be used in there
#' are structural zeros in the matrix of observed
#' transition frequencies.
#' @export
#'


seq_exp<-function(x){

  obs<-seq_obs(x)

expected <- function(mat){
  N<-nrow(mat) #rows of matrix
  myFun <- function(z,y,M){(sum(M[z,]) * sum(M[,y]))/sum(M)}
  myVecFun <- Vectorize(myFun,vectorize.args = c('z','y'))
  outer(1:N,1:N,myVecFun,mat)
}

expt<-expected(obs)
rownames(expt)<-colnames(expt)<-rownames(obs)
return(expt)
}

