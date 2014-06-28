\name{fromBIOM}
\alias{fromBIOM}
\alias{as.matrix.biom}
\alias{as.list.biom}
\alias{as.character.biom}

\title{
Convert BIOM data from formal to basic type (export)
}

\description{
Convert an object of class "biom" into a basic type:  "matrix", "list", or "character".
}

\usage{
\method{as.matrix}{biom}(x, ...)
\method{as.list}{biom}(x, ...)
\method{as.character}{biom}(x, ...)
}

\arguments{
  \item{x}{an object ("biom")}
  \item{\dots}{unused}
}

\details{
In genomics, Biological Observation Matrix (BIOM) is a simple set of annotations
for a data table expressed in JSON.  BIOM can be described as a format, standard,
or even data structure.  Familiarity with BIOM is assumed, here.  For handling BIOM
data in R, there is the formal class "biom".

These functions each transform an object that is already of class "biom", into a basic R type.

"as.matrix" returns the BIOM data table as a "matrix", with "dimnames" equal to the
BIOM row and column "id"s.  "as.character" returns BIOM properly speaking --- that is, 
it returns (a length-one "character" vector of) text that encodes data and annotations in JSON
following BIOM.

"as.list" returns a "list", closely corresponding element-by-element to BIOM.  
The differences are:  "data" is a "matrix", not a "list"; its
"dimnames" hold the row and column "id"s;  and consequently, "rows" and "columns" do not hold
"id"s, but only "metadata".
}

\value{
As described.
}

\seealso{
"\link{biom}",
"\link{as.matrix}",
"\link{as.list}",
"\link{as.character}"
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

