% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/seq_trans.R
\name{seq_trans}
\alias{seq_trans}
\title{Get transition probabilities of transitions}
\usage{
seq_trans(x, l = 1)
}
\arguments{
\item{x}{a character sequence}

\item{l}{the lag}
}
\value{
Table of transition probabilities
}
\description{
Get transition probabilities of transitions
}
\section{Further details}{

Generates a table of transition probabilities
at specified lag
}
\examples{
seq_trans("abacbabacbabcababacbcbcbabaaabababbababcb")
seq_trans("abacbabacbabcababacbcbcbabaaabababbababcb", 2)
}

