\name{print.biom}
\alias{print.biom}
\alias{summary.biom}

\title{Print or summarize BIOM object}
\description{
Nicely print or summarize an object of class \code{biom}.  The difference is whether
the object's data table is printed.}

\usage{
\method{print}{biom}(x, ...)
\method{summary}{biom}(object, ...)}

\arguments{
  \item{x}{an object (\code{biom})}
  \item{object}{an object (\code{biom})}
  \item{\dots}{unused}}

\details{
Nicely print or summarize an object of class \code{biom}.
The difference is whether the object's data table is printed.
\code{summary} does not print that, but prints just about everything else the object contains.}

\value{
Nothing useful.}

\author{
Daniel T. Braithwaite}

\seealso{
\code{\link{biom}}}

\examples{
if(require(MGRASTer)) {
	cc <- call.MGRAST('ma', 'or', 
		id=c(4447943.3, 4447192.3, 4447102.3, 4447103.3), group='family')
# a warning in the next step is normal
	bb <- biom(cc)
	print(bb)
	summary(bb)
}
}

