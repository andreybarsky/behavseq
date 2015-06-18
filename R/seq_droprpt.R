#' Drop repeated elements from a sequence
#' @param x a vector or character sequence
#' @param coll whether input sequence is a
#' vector sequence  \strong{\code{coll}=F} or a
#' character sequence  \strong{\code{coll}=T}
#' @return A character or vector sequence with no
#' repeated elements
#' @examples
#' seq_droprpt(c("A", "A", "A", "B", "C","C", "C"))
#' seq_droprpt('AAABBBCCCDDDEEEBBBAAA', coll=T)
#' @section Further details:
#' Drops repeating elements from a vector or
#' character sequence
#' @export


seq_droprpt <- function(x, coll=F) {

  if(coll==F){
    if(length(x) == 1) {return(x)}

    x <- as.vector(x)
    v <- c(x[1])
    for(i in 2:length(x)) {
      if(x[i] != v[length(v)]) {
        v <- c(v, x[i])
      }
    }
    return(v)
  }

  else

    if(coll==T){

      x <- uncollapse(x)

      if(length(x) == 1) {return(x)}

      x <- as.vector(x)
      v <- c(x[1])
      for(i in 2:length(x)) {
        if(x[i] != v[length(v)]) {
          v <- c(v, x[i])
        }
      }

      v1<-collapse(v)
      return(v1)

    }
}

