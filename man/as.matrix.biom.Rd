\name{as.matrix.biom}
\alias{as.matrix.biom}

\title{Extract matrix from BIOM}
\description{
The data table stored in an object of class \code{biom} is extracted and returned as an 
object of class \code{matrix} or \code{Matrix}.}

\usage{
\method{as.matrix}{biom}(x, alt.names=FALSE, force.dense=FALSE, ...)}

\arguments{
  \item{x}{an object (\code{biom})}
  \item{alt.names}{use row and column names from the \code{metadata} fields of \code{x}?  (\code{logical})}
  \item{force.dense}{if sparse, convert? (\code{logical})}
  \item{\dots}{unused}}


\details{
BIOM format requires the fields \code{id} and \code{metadata} per row and per column.
By default, \code{id} is used to give row and column names to the matrix returned by this function.
With \code{alt.names=TRUE}, \code{metadata} is used instead.
Components of a compound \code{metadata} field are concatenated and comma-separated.
\code{alt.names} may be length-2 to specify different behavior for rows and columns.
See also \code{dim.biom}.

The value of \code{force.dense} only matters when \code{x$sparse=TRUE}.  
In that case, if \code{force.dense=FALSE}, the returned object is a sparse matrix as defined in package \code{Matrix}.}

\value{
A matrix (class \code{matrix} or \code{Matrix}) containing the data table stored in the given BIOM object.}

\author{
Daniel T. Braithwaite}

\seealso{
\code{\link{biom}}, \code{\link{dim.biom}}}

\examples{
if(require(MGRASTer)) {
	cc <- call.MGRAST('ma', 'or', 
		id=c(4447943.3, 4447192.3, 4447102.3, 4447103.3), group='family')
# a warning in the next step is normal
	bb <- biom(cc)
	mm <- as.matrix(bb)
# it is a sparse matrix
	class(mm)
	head(mm)
	dimnames(mm)
# now dense, with better row names
	mm <- as.matrix(bb,alt=c(TRUE,FALSE),force=TRUE)
	class(mm)
	dimnames(mm)
}
}

