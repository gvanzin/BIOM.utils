\name{is.biom}
\alias{is.biom}

\title{Check and repair BIOM format}
\description{
Check whether an object is of class \code{biom} and properly conforms to BIOM format.
Optionally, attempt to repair it.}

\usage{
is.biom(x, fix=FALSE, check.all=fix, quiet=!check.all)}

\arguments{
  \item{x}{an object (\code{biom})}
  \item{fix}{attempt repair? (\code{logical})}
  \item{check.all}{conduct thorough check? (\code{logical})}
  \item{quiet}{check without diagnostic messages? (\code{logical})}}

\details{
A few shortcuts are taken when \code{check.all=FALSE}, but otherwise, the object is thoroughly validated.  
A good result can not be guaranteed by \code{fix=TRUE}, 
and data may be lost, but small deviations from BIOM format can be repaired.}

\value{
An error is signaled if \code{fix=TRUE} and the object can't be repaired.
Otherwise, the value is \code{TRUE} or \code{FALSE}.}

\author{
Daniel T. Braithwaite}

\seealso{
\code{\link{biom}}}

\examples{#}

