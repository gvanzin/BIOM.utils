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
Report \code{shape} (dimensions) of an object of class \code{biom}.  Report unique row and column \code{id}s.  
Report \code{metadata} from \code{rows} and \code{columns} fields.
}

\usage{
\method{dim}{biom}(x)
\method{dimnames}{biom}(x)
rows(x)
columns(x)
}

\arguments{
  \item{x}{an object (\code{biom})}
}

\details{
In genomics, Biological Observation Matrix (BIOM) is a simple set of annotations
for a data table expressed in JSON.  BIOM can be described as a format, standard,
or even data structure.  Familiarity with BIOM is assumed, here.  For handling BIOM
data in R, there is the class \code{biom}.

These functions apply to an R object that is already of class \code{biom}.

\code{dim()} returns the dimensions of the BIOM data table (its \code{shape}, in BIOM terminology).  
\code{dimnames()} returns the unique row and column \code{id}s that are required by BIOM.
\code{rows()} (or \code{columns()}) returns a list of the row (or column) \code{metadata} annotations provisioned by
BIOM.
}

\value{
As described.
}

\seealso{
\code{\link{biom}},
\code{\link{dim}},
\code{\link{dimnames}}
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
#
}

