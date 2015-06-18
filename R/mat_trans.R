#' Get transition probabilities from observed frequency matrix
#' @param m a matrix or table of observed transition frequencies
#' @return Table of observed frequencies
#' @examples
#' a<-seq_obs("ababababcdcdbabdcbadbcadbcadcb")
#' mat_trans(a)
#' @section Further details:
#' Get transition probabilities from observed frequency matrix
#' @export
#'

mat_trans<- function(m) {m/(rowSums(m))}
