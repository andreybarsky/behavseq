#' Get simple conditional probailities of behaviors
#' @param x a character sequence
#' @return A vector of conditional probabilities
#' @examples
#' prob_con("abacbabacbabcababacbcbcbabaaabababbababcb")
#' prob_con("AABABABBABAB")
#' prob_con(jaffe) #on of the example sequences
#' @section Further details:
#' Calculates simple conditional probabilties of behaviors in
#' a character sequence at a lag of 1.
#' @export

prob_con <- function(x){

  codes <- unique(uncollapse(x))
  v  <-vector(mode="numeric", length=nrow(expand.grid(codes,codes)))
  names(v)<-apply(expand.grid(codes,codes),  1, function(x) paste0(x, collapse=''))

  v1<-seq_substrings(x,2)
  v2<-as.vector(v1)
  names(v2)<-names(v1)

  vv <- tapply(c(v,v2), names(c(v,v2)), sum)

  freq<-table(uncollapse(x))
  conp <- vv / freq[match(sapply(names(vv),substr,2,2), names(freq))]
  return(conp)
}
