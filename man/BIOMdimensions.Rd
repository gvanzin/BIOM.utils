\name{BIOMdimensions}
\alias{BIOMdimensions}
\alias{dim.biom}
\alias{dimnames.biom}
\alias{rows}
\alias{columns}

\title{
Row and column information of BIOM data
}

\description{
Report "shape" (dimensions) of an object of class "biom".  Report unique row and column "id"s.  
Report "metadata" from "rows" and "columns" fields.
}

\usage{
\method{dim}{biom}(x)
\method{dimnames}{biom}(x)
rows(x)
columns(x)
}

\arguments{
  \item{x}{an object ("biom")}
}

\details{
In genomics, Biological Observation Matrix (BIOM) is a simple set of annotations
for a data table expressed in JSON.  BIOM can be described as a format, standard,
or even data structure.  Familiarity with BIOM is assumed, here.  For handling BIOM
data in R, there is the formal class "biom".

These functions apply to an R object that is already of class "biom".

"dim" returns the dimensions of the BIOM data table (a.k.a its "shape" in BIOM terminology).  
"dimnames" returns the unique row and column "id"s that are required by BIOM.
"rows" ("columns") returns a list of the row (column) "metadata" annotations provisioned by
BIOM.
}

\value{
As described.
}

\seealso{
"\link{biom}",
"\link{dim}",
"\link{dimnames}"
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
	dimnames(bb)
	dim(bb)
}
}

