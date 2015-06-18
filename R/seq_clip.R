#' Extract a sequence at specific points
#' @param x a character sequence
#' @param b the beginning position of extracted sequence
#' @param e the end position of extracted sequence
#' @return An extracted subsequence of original sequence
#' @examples
#' seq_clip("abacbabacbabcababacbcbcbabaaabababbababcb", b=4,e=10)
#' @section Further details:
#' Clip a sequence to extract a subsequence
#' @export
#'

seq_clip <- function(x,b=1,e=1){ collapse(uncollapse(x)[b:e]) }

