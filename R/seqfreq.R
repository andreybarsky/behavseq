
#' Get observed frequncy of a behavior
#' @param x a character sequence
#' @return A vector of observed frequencies
#' @examples
#' seqfreq("abacbabacbabcababacbcbcbabaaabababbababcb")
#' @section Further details:
#' Calculates simple observed frequencies of behaviors in
#' a character sequence
#' @export


seqfreq <-function(x) { table(uncollapse(x)) }

