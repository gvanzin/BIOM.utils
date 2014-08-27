\name{BIOM Dimensions}
\alias{dim.biom}
\alias{dimnames.biom}
\alias{metadata}
\alias{metadata.biom}

\title{
Row and column information of BIOM data
}

\description{
Report \code{shape}, 
unique row and column \code{id}s,
and row and column \code{metadata} annotations 
of an object of class \code{biom}.
}

\usage{
\method{dim}{biom}(x)

\method{dimnames}{biom}(x)

metadata(x, ...)
\method{metadata}{biom}(x, ...)
}

\arguments{
  \item{x}{an object (\code{biom})}
  \item{...}{unused}
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
The functions above apply to an R object that is already of class \code{biom}.

\code{dim()} returns the dimensions of the BIOM data table (its \code{shape}).

\code{dimnames()} returns the unique row and column \code{id}s required by BIOM.

\code{metadata()} returns the row and column \code{metadata} annotations 
provided for by BIOM.  They may be empty.
}

\value{
For \code{dim()}, a length-two \code{integer} vector, 
with additional attribute \code{"nnz"} (number not zero)
if the object is \code{"sparse"}, 
equaling the number of values (rows) in the sparse representation.  
See note below.

For \code{dimnames()}, a \code{list} of two \code{character} vectors,
named \code{"rows"} and \code{"columns"}.

For \code{metadata()}, a \code{list} (invisibly) of two components,
named \code{"rows"} and \code{"columns"}.  
Each is a \code{list} equal in length to the data table's corresponding dimension.
}

\note{
When \code{x} is \code{"sparse"}
the dimensions of \code{as.matrix(x)} are not given by \code{dim(x)}.
Rather, the latter has dimensions \code{c("nnz", 3)}.  See above.

Also note that BIOM requires no exact structure 
for the metadata annotations of each row (column).
All that is guaranteed is a list 
(possibly of empty, atomic, or nested list elements)
with as many entries as the data table has rows (columns).
}

\seealso{
\code{\link{biom}},
\code{\link{as.matrix.biom}}
}

\author{
Daniel T. Braithwaite
}

\references{
\href{http://wwww.biom-format.org}{BIOM format} \cr
\href{http://wwww.json.org}{JSON} \cr
}


\examples{
##  one toy example, one real example:
dd <- biom (dmat, quiet=TRUE)
ff <- biom (li4, quiet=TRUE)

dim (dd)
dim (ff)
dim (as.matrix (ff))
dim (as.matrix (ff, expand=TRUE))

dimnames (dd)
dimnames (ff)

##  automatic row and column ids:
dimnames (biom (smat, sparse=TRUE, quiet=TRUE))

##  no metadata:
print (metadata (dd))

##  simple metadata:
ss <- biom (li3)
print (as.matrix (ss))
metadata (ss) $ columns

##  complicated metadata, so look only at part:
head (metadata(ff) $ rows)
str (metadata(ff) $ columns [[1]])
}
