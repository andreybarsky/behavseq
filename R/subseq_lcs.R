#' Find the longest common subsequence to two sequences
#' @param x character sequence 1
#' @param y character sequence 2
#' @param \strong{\code{type}="df"} return a dataframe
#' \strong{\code{type}="v"} return a vector
#' @return Longest common subsequence in a dataframe with
#' indices of each behavior in x and y in columns or in a vector
#' @examples
#' subseq_lcs('asdfasdfasdfasdf', 'asdgdsfagasdfsg')
#' @section Further details:
#' Find the longest common subsequence to two sequences.  The two
#' types utilize different methods and may produce varying solutions -
#' there is not necessarily one unique solution
#' @export
#'


subseq_lcs <- function(x, y, type="df") {

  if(type=="df"){

  X<-uncollapse(x)
  Y<-uncollapse(y)

  m <- length(X)
  n <- length(Y)
  C <- matrix(0, 1 + m, 1 + n)
  for (i in seq_len(m)) {
    for (j in seq_len(n)) {
      if (X[i] == Y[j]) {
        C[i + 1, j + 1] = C[i, j] + 1
      } else {
        C[i + 1, j + 1] = max(C[i + 1, j], C[i, j + 1])
      }
    }
  }

  backtrack <- function(C, X, Y, i, j) {
    if (i == 1 | j == 1) {
      return(data.frame(I = c(), J = c(), LCS = c()))
    } else if (X[i - 1] == Y[j - 1]) {
      return(rbind(backtrack(C, X, Y, i - 1, j - 1),
                   data.frame(LCS = X[i - 1], I = i - 1, J = j - 1)))
    } else if (C[i, j - 1] > C[i - 1, j]) {
      return(backtrack(C, X, Y, i, j - 1))
    } else {
      return(backtrack(C, X, Y, i - 1, j))
    }
  }

  df <- backtrack(C, X, Y, m + 1, n + 1)
  colnames(df)<-c("LCS", "x", "y")
  return(df)
  }

  else

if(type=="v"){

  subseq_LCS<-function(x,y){

    sapply(seq_along(x), function(i)
      paste(qualV::LCS(substring(x[i], seq(1, nchar(x[i])), seq(1, nchar(x[i]))),
                substring(y[i], seq(1, nchar(y[i])), seq(1, nchar(y[i]))))$LCS,
            collapse = ""))
  }

  return(subseq_LCS(x,y))

}
}
