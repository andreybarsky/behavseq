#' Get timed-interval behavioral sequence from a dataframe of behavioral observations
#' @param df dataframe containing behavioral observations
#' @param begin time at which to start sequence
#' @param end time at which to end sequence
#' @param interval the time unit inteval over which to add successive codes
#' @param varx character defining name of column to make into a sequence
#' @param vary character defining name of column containing time values
#' @param collapse whether to make sequence a character sequence (default)
#' or a  vector sequence (if collpase=F)
#' @return A character of vector sequence
#' @examples
#' get_seq0(mouse, varx = 'behaviorX', begin=1150, end=1200, interval=1)
#' @section Further details:
#' Get timed-interval  behavioral sequence from a dataframe of behavioral observations
#' @export
#'

get_seq0 <- function(df, begin=1, end=1199, interval=1, varx='state', vary='time', collapse=T){

  TIME <- df[,vary]
  STATE <- df[,varx]

  x <- as.numeric(cut(seq(begin,end,interval), breaks=TIME, right=FALSE))
  vec<-replace(x, is.na(x), length(TIME))
  myseq <- STATE[vec]

  if(collapse==F) {
    return(myseq)
  }

  else

    myseq1 <- paste0(myseq,collapse="")
  return(myseq1)

}


