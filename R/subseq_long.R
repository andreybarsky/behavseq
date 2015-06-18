#' Find length of longest subsequence common to two sequences
#' @param x character sequence 1
#' @param y character sequence 2
#' @return Length of longest subsequence common to both
#' @examples
#' subseq_long("cacaabdcabdcab", "andcabdcadcad")
#' @section Further details:
#' Find length of longest subsequence common to two sequences
#' @export
#'

subseq_long <- function(x, y) {

  s1 <- unlist(strsplit(x,split=""))
  s2 <- unlist(strsplit(y,split=""))

  num <- matrix(0,nchar(x),nchar(y) )
  maxlen <- 0

  for (i in 1:nchar(x)) {

    for (j in 1:nchar(y)) {

      if (s1[i] == s2[j]) {
        if ((i==1) || (j==1)) {
          num[i,j] <- 1
        }
        else {
          num[i,j] <- 1+num[i-1,j-1]
        }
        if (num[i,j] > maxlen) {
          maxlen <- num[i,j]
        }
      }
    }
  }

  return(maxlen)
}



