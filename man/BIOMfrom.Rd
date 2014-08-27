\name{BIOM Export}
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
\method{as.matrix}{biom}(x, expand=NULL, ...)

\method{as.list}{biom}(x, ...)

\method{as.character}{biom}(x, ..., file=NULL)
}

\arguments{
  \item{x}{an object (\code{biom})}
  \item{expand}{if sparse, force return of not sparse matrix?  (\code{logical})}
  \item{\dots}{unused}
  \item{file}{filename for writing object}
}

\details{
BIOM (Biological Observation Matrix) is a simple prescription 
for storing an annotated table of data.
It may be described as a format, standard, or data structure.

The JSON (JavaScript Object Notation) standard 
for expressing general data objects as text
is employed to define BIOM.
Therefore the native form of BIOM data is structured text,
conforming to the JSON specification in general
and the BIOM specification in particular.
Familiarity with BIOM is assumed here.

The S3 class \code{biom} and its methods facilitate analyses
by expressing BIOM data as objects in the R environment.
Each function above transforms an object that is 
already of class \code{biom} into a basic R type.

\code{as.matrix()} returns the BIOM data table as a \code{matrix}.
If the object is \code{"dense"}, then \code{dimnames()} of the result
are equal to the BIOM row and column \code{id}s.
Otherwise, the three-column sparse representation matrix is returned, 
with \code{ids} given by attached attributes \code{"rownames"} and \code{"colnames"}.

However, using \code{expand=TRUE} expands a sparse representation.  
(Setting \code{expand=FALSE} has no effect when the object is \code{"dense"}.)
Also, see below for an example using class "sparseMatrix" 
from the R package Matrix.

\code{as.character()} returns BIOM properly speaking, that is, 
data and annotations written in JSON text conforming to the BIOM specification.
That text is written to \code{file}, if provided.

See below for an example of saving the data table, only, in CSV or TSV format.
Of course, it is possible to bring \code{biom} objects
in and out of the session with \code{save()} and \code{load()}.

\code{as.list()} returns a \code{list} corresponding closely 
element-by-element to BIOM.  The differences are:
list element \code{data} is a \code{matrix} not \code{list}; 
elements \code{rows} and \code{columns} hold \code{metadata} only,
and so do not include \code{id}s; instead, additional elements 
\code{row.ids} and \code{column.ids} are present;
and \code{format_url} is missing.
}

\value{
For \code{as.matrix()} and \code{as.list()}, as described.

For \code{as.character()}, a single string of JSON text,
or simply \code{file} if it is not {NULL}.

Except that last case, these functions return invisibly.
}

\seealso{
\code{\link{biom}}
}

\author{
Daniel T. Braithwaite
}

\references{
\href{http://wwww.biom-format.org}{BIOM format} \cr
\href{http://wwww.json.org}{JSON} \cr
}

\examples{
tt <- tempfile()
xx <- biom (li3)
yy <- biom (smat, sparse=TRUE, quiet=TRUE)

##  extract objects of basic types:
print (as.matrix (xx))
head (as.matrix (yy, expand=TRUE))
as.character (xx)
as.character (xx, file=tt)
str (as.list (xx))

##  export to a CSV file:
write.table (as.matrix (xx), file=tt, sep=",")

\dontrun{
##  a classed sparse matrix (for computation or what have you):
zz <- as.matrix (yy)
zz[,1:2] <- 1 + zz[,1:2]
Matrix::sparseMatrix (i=zz[,1], j=zz[,2], x=zz[,3])
}

unlink(tt)
}
