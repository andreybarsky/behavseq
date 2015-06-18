#' Uncollapse a vector sequence
#' @param x a character sequence
#' @param n The number of codes to put into each distinct
#' element.
#' @return A vector sequence
#' @examples
#' uncollapse("ABCDEFGABCDEFG")
#' @section Further details:
#' Converts a character sequence to a vector sequence
#' @export

uncollapse <-function(x,n=1){
  substring(x, seq(1,nchar(x),n), seq(n,nchar(x),n))
}


