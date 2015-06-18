#' Get all subsequences of a given length
#' @param x a character sequence
#' @param n length of substrings
#' @return A vector of all possible substrings
#' @examples
#' seq_substrings("abacbabacbabcababacbcbcbabaaabababbababcb")
#' @section Further details:
#' Calculates all possible substrings of a given length in
#' a character sequence
#' @export

seq_substrings<-function(x,n=2){
  N<-pmin(nchar(x)-n, n)
  res<-NULL
  xx<-NULL
  xx[1]<-x

  for(i in 1:N){

    res[[i]] <- substring(xx[i], seq(1,nchar(xx[i]),n), seq(n,nchar(xx[i]),n))
    xx[[i+1]]  <- substr(xx[i],2,nchar(xx[i]))

  }

  results <- unlist(res)[unlist(res)!=""]
  return(table(results))
}
