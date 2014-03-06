\name{as.character.biom}
\alias{as.character.biom}

\title{Convert BIOM to JSON text}
\description{
Convert an object of class \code{biom} to JSON text, the native syntax for BIOM format.}

\usage{
\method{as.character}{biom}(x, ...)}

\arguments{
  \item{x}{an object (\code{biom})}
  \item{\dots}{arguments for \code{toJSON}}}

\details{
This function applies \code{toJSON()} from package \code{RJSONIO}.
Arguments in \code{...} are passed along uncritically, 
except that \code{pretty=TRUE} is already hardwired.
}

\value{
A (length-one) character vector of JSON text or an error as returned by \code{toJSON()}.}

\author{Daniel T. Braithwaite}
\seealso{
\code{\link{biom}}}

\examples{
if(require(MGRASTer)) {
	cc <- call.MGRAST('ma', 'or', 
		id=c(4447943.3, 4447192.3, 4447102.3, 4447103.3), group='family')
# a warning in the next step is normal
	bb <- biom(cc)
	tt <- as.character(bb)
	str(tt)
}
}
