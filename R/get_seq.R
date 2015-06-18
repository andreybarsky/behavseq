#' Get behavioral sequence from a dataframe of behavioral observations
#' @param df dataframe containing behavioral observations
#' @param begin time at which to start sequence
#' @param end time at which to end sequence
#' @param varx character defining name of column to make into a sequence
#' @param vary character defining name of column containing time values
#' @param drop whether to include repeated behavior codes
#' @param collapse whether to make sequence a character sequence (default)
#' or a  vector sequence (if collpase=F)
#' @return A character of vector sequence
#' @examples
#' get_seq(mouse, varx = 'stateX', begin=1100, end=1200)
#' get_seq(mouse, varx = 'stateX', drop=T, begin=1100, end=1200)
#' @section Further details:
#' Get behavioral sequence from a dataframe of behavioral observations
#' @export
#'

get_seq <- function(df, begin=0,end=1200, varx='state', vary='time', drop=F, collapse=T){

  library(dplyr)

  tt <- df %>% filter(df[,vary] >= begin & df[,vary] <=end)
  tt <- as.character(tt[,varx])

  if(collapse==F & drop==F) { return(tt) }
  if(collapse==T & drop==F) { return(paste0(tt,collapse=""))}

  if(collapse==F & drop==T) {
    ttx <- tt[!tt==lag(tt)]
    ttx[1] <- tt[1]
    return(ttx)
  }
  if(collapse==T & drop==T) {
    ttx <- tt[!tt==lag(tt)]
    ttx[1] <- tt[1]
    return(paste0(ttx,collapse=""))
  }
}
