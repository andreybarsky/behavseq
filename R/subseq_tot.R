#' Find indices of subsequences in a given subsequence
#' @param x subsequence to test for
#' @param y sequence to compare to
#' @return Total number of instances of subsequence,
#' the indices of the subsequences in the sequence,
#' the length of the sequence, the frequency of the
#' subsequence
#' @examples
#' subseq_tot("ab","adcabdadabdadabdabd")
#' @section Further details:
#' Find indices of subsequences in a given subsequence
#' @export
#'

subseq_tot<-function(x, y){
  a<-uncollapse(x)
  b<-uncollapse(y)

  indices<- which(zoo::rollapply(b, length(a), identical, a))
  total<-length(indices)
  freq <- total/(length(b)*(length(b)+1)/2)

  cat("Total = ", total, "\n", "Indices = ", indices,
      "\n", "Seq Length = ", length(b), "\n", "Frequency = ", freq)
}

