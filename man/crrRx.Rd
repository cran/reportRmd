% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{crrRx}
\alias{crrRx}
\title{fit crr model}
\usage{
crrRx(f, data)
}
\arguments{
\item{f}{formula for the model. Currently the formula only works by using the
name of the column in a dataframe. It does not work by using $ or []
notation.}

\item{data}{dataframe containing data}
}
\value{
a competing risk model with the call appended to the list
}
\description{
Wrapper function to fit fine and gray competing risk model using function crr
from package cmprsk
}
\examples{
# From the crr help file:
set.seed(10)
ftime <- rexp(200)
fstatus <- sample(0:2,200,replace=TRUE)
cov <- matrix(runif(600),nrow=200)
dimnames(cov)[[2]] <- c('x1','x2','x3')
df <- data.frame(ftime,fstatus,cov)
m1 <- crrRx(as.formula('ftime+fstatus~x1+x2+x3'),df)
# Nicely output to report:
rm_mvsum(m1,data=df,showN = TRUE,vif=TRUE)
}
\seealso{
\code{\link{crr}}
}
\keyword{model}
