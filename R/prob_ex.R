#' Get unconditional probability of a behavior
#' @param x a character sequence
#' @return A vector of unconditional probabilities
#' @examples
#' prob_ex("abacbabacbabcababacbcbcbabaaabababbababcb")
#' @section Further details:
#' Calculates simple unconditional probabilities of behaviors in
#' a character sequence
#' @export

prob_ex <-function(x) { table(uncollapse(x)) / length(uncollapse(x)) }

