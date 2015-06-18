#' Find the longest subsequence(s) common to two sequences
#' @param x character sequence 1
#' @param y character sequence 2
#' @return Longest subsequence(s) common to both
#' @examples
#' subseq_long0("ababdcbdabdbcabadbcbabbcbdabc", "dbadbcdbadbcaddbdddbc")
#' @section Further details:
#' Find the longest subsequence(s) common to two sequences
#' @export
#'

subseq_long0<-function(x,y){

n<-subseq_long(x,y)
names(seq_substrings(x,n))[names(seq_substrings(x,n)) %in% names(seq_substrings(y,n))]

}
