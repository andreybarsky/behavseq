#' Split a sequence into roughly equal parts
#' @param x a character sequence
#' @param n the number of parts to split the sequence into
#' @return A list of split character sequences
#' @examples
#' seq_split("abacbabacbabcababacbcbcbabaaabababbababcb", 2)
#' seq_split("abacbabacbabcababacbcbcbabaaabababbababcb", 3)
#' @section Further details:
#' Split a character sequence into roughly equal parts
#' @export
#'

seq_split<-function(x,n=2){
  N<-nchar(x)
  seq1x<-uncollapse(x)
  s<-split(seq1x, ceiling(seq_along(seq1x)/(N/n)))
  ls<-lapply(s,collapse)
  return(ls)
}

