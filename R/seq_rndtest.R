#' Randomization test of behavioral contingencies in a sequence
#' @param x a character sequence
#' @param nx length of substrings
#' @param nperms number of permutations
#' @return A vector of probabilities for each contingency
#' @examples
#' seq_rndtest("abacbabacbabcababacbcbcbabaaabababbababcb",2)
#' seq_rndtest("abacbabacbabcababacbcbcbabaaabababbababcb",3)
#' @section Further details:
#' Performs a randomization test to determine which contingencies
#' occur at an observed frequency higher than chance.
#' @export



### generic function of above:
seq_rndtest<-function(seqz, nx=2, nperms=10000){

  obs <- seq_substrings(seqz,nx)

  res<-NULL

  for(i in 1:nperms){
    res[[i]] <- seq_substrings(collapse(sample(uncollapse(seqz))),nx)
  }

  tmp<-do.call('c', (lapply(res, unlist)))

  resz <-NULL

  for(i in 1:length(obs)){
    resz[[i]] <- sum(tmp[names(tmp)==names(obs[i])]>=as.numeric(obs[i]))/nperms
  }

  names(resz)<-names(obs)
  return(resz)

}
