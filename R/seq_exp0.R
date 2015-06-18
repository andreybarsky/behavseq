#' Get expected frequencies of first order transitions for
#' non-repeating sequences
#' @param x a character sequence with repeated behaviors not allowed
#' @return Table of observed frequencies
#' @examples
#' seq_exp0(seq_droprpt("abacbabacbabcababacbcbcbabaaabababbababcb",coll=T))
#' @section Further details:
#' This function uses iterative proportional fitting to find expected frequencies
#' for non-repeating  sequences
#' @export
#'


seq_exp0<-function(x){

  ## THIS IS FOR STRUCTURAL ZEROS ALONG THE DIAGONAL

  mat<-seq_obs(x)

  NN<-nrow(mat)
  library(mipfp)
  seed.2d <- array(1,dim=c(NN,NN)) # desired targets (margins) : V1 and V2
  diag(seed.2d)<-0
  target.row <- rowSums(mat)
  target.col <- colSums(mat)
  tgt.data.2d <- list(target.row, target.col) # storing the margins in a list
  tgt.list.2d <- list(1,2) # list of dimensions of each marginal constrain

  # calling the Ipfp function, requesting the covariance matrices
  res.2d <- mipfp::Ipfp(seed.2d, tgt.list.2d, tgt.data.2d, compute.cov = TRUE)
  out <- res.2d$x.hat
  rownames(out)<-colnames(out)<-rownames(mat)
  return(out)
}
