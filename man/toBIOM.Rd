\name{toBIOM}
\alias{toBIOM}
\alias{biom}
\alias{biom.matrix}
\alias{biom.list}
\alias{biom.character}

\title{
Attach formal type to BIOM data (import)
}

\description{
Construct an object of class "biom" from given data, supplementing missing fields as 
necessary.
}

\usage{
biom(x, ...)
\method{biom}{list}(x, ..., quiet=FALSE)
\method{biom}{matrix}(x, type=biom_table_types, ..., quiet=FALSE)
\method{biom}{character}(x, ..., file=NULL, quiet=FALSE)
}

\arguments{
  \item{x}{an object convertible to "biom"}
  \item{type}{the type of BIOM table to be constructed (string)}
  \item{\dots}{unused}
  \item{file}{file containing JSON text for BIOM data (string)}
  \item{quiet}{print messages and warnings? ("logical")}
}

\details{
In genomics, Biological Observation Matrix (BIOM) is a simple set of annotations
for a data table expressed in JSON.  BIOM can be described as a format, standard,
or even data structure.  Familiarity with BIOM is assumed, here.  For handling BIOM
data in R, there is the formal class "biom".

These functions each transform objects of a basic R type into class "biom".

The "character" method is suitable for importing BIOM data properly speaking, that is, 
text in JSON encoding that fits the BIOM specification.  The "matrix" method creates an 
object of the formal class "biom" from an existing "matrix" object.

The "list" method accepts a "list" of any of the components allowed or required by BIOM, 
with minor differences:
"data" may be a "list" of rows, a "list" of triples sparsely representing a matrix, or an
actual "matrix" object; 
the dimnames of the "matrix" may be used to provide the row and column "id"s; and if so, 
"rows" and "columns" may hold "metadata" only, without "id"s.
(This mostly describes the internal representation of the "biom" class.)  
An attempt will be made to provide components of BIOM that are missing.
}

\value{
An object of class "biom", except in case of error.
}

\seealso{
"\link{as.character.biom}",
"\link{as.matrix.biom}",
"\link{dim.biom}",
"\link{is.biom}",
"\link{print.biom}"
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
