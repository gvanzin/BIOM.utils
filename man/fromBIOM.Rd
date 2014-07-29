\name{fromBIOM}
\alias{fromBIOM}
\alias{as.matrix.biom}
\alias{as.list.biom}
\alias{as.character.biom}

\title{
Convert BIOM data from formal to basic type (export)
}

\description{
Convert an object of class \code{biom} into a basic type:  \code{matrix}, \code{list}, or \code{character}.
}

\usage{
\method{as.matrix}{biom}(x, ...)
\method{as.list}{biom}(x, ...)
\method{as.character}{biom}(x, ...)
}

\arguments{
  \item{x}{an object (\code{biom})}
  \item{\dots}{unused}
}

\details{
In genomics, Biological Observation Matrix (BIOM) is a simple set of annotations
for a data table expressed in JSON.  BIOM can be described as a format, standard,
or even data structure.  Familiarity with BIOM is assumed, here.  For handling BIOM
data in R, there is the formal class \code{biom}.

These functions each transform an object that is already of class \code{biom}, into a basic R type.

\code{as.matrix()} returns the BIOM data table as a \code{matrix}, with \code{dimnames} equal to the
BIOM row and column \code{id}s.  \code{as.character()} returns BIOM properly speaking --- that is, 
it returns (a length-one \code{character} vector of) text that encodes data and annotations in JSON
following BIOM.

\code{as.list()} returns a \code{list}, closely corresponding element-by-element to BIOM.  
The differences are:  \code{data} is a \code{matrix}, not a \code{list}; its
\code{dimnames} hold the row and column \code{id}s;  and consequently, \code{rows} and \code{columns} do not hold
\code{id}s, but only \code{metadata}.
}

\value{
As described.
}

\seealso{
\code{\link{biom}},
\code{\link{as.matrix}},
\code{\link{as.list}},
\code{\link{as.character}}
}

\author{
Daniel T. Braithwaite
}

\references{
\href{http://wwww.biom-format.org}{BIOM format} \cr
\href{http://metagenomics.anl.gov}{MG-RAST metagenome annotation server} \cr
\href{http://wwww.json.org}{JSON} \cr
}


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

