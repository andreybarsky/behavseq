#' Check if a sequence contains a given subsequence
#' @param x subsequence to test for
#' @param y sequence to compare to
#' @return Logical T/F
#' @examples
#' subseq_in("ab", "adbcdbacbdabcd")
#' subseq_in("abd", "adbcdbacbdabcd")
#' @section Further details:
#' Check if a sequence contains a given subsequence
#' @export
#'

subseq_in <- function(x, y){
  stringi::stri_detect_fixed(str = paste0(y, collapse = ","), pattern = paste0(x, collapse = ","))
}



