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
Construct an object of class \code{biom} from given data, supplementing missing fields as 
necessary.
}

\usage{
biom(x, ...)
\method{biom}{list}(x, ..., quiet=FALSE)
\method{biom}{matrix}(x, type=biom_table_types, ..., quiet=FALSE)
\method{biom}{character}(x, ..., file=NULL, quiet=FALSE)
}

\arguments{
  \item{x}{an object convertible to \code{biom}}
  \item{type}{the type of BIOM table to be constructed (string)}
  \item{\dots}{unused}
  \item{file}{file containing JSON text for BIOM data (string)}
  \item{quiet}{print messages and warnings? (\code{logical})}
}

\details{
In genomics, Biological Observation Matrix (BIOM) is a simple set of annotations
for a data table expressed in JSON.  BIOM can be described as a format, standard,
or even data structure.  Familiarity with BIOM is assumed, here.  For handling BIOM
data in R, there is the class \code{biom}.

These functions each transform objects of a basic R type into class \code{biom}.

The \code{character} method is suitable for importing BIOM data properly speaking, that is, 
text in JSON encoding that fits the BIOM specification.  The \code{matrix} method creates an 
object of the formal class \code{biom} from an existing \code{matrix} object.

The \code{list} method accepts a \code{list} of any of the components allowed or required by BIOM, 
with minor differences:
\code{data} may be a \code{list} of rows, a \code{list} of triples sparsely representing a matrix, or an
actual \code{matrix} object; 
the dimnames of the \code{matrix} may be used to provide the row and column \code{id}s; and if so, 
\code{rows} and \code{columns} may hold \code{metadata} only, without \code{id}s.
(This mostly describes the internal representation of the \code{biom} class.)  
An attempt will be made to provide components of BIOM that are missing.
}

\value{
An object of class \code{biom}, except in case of error.
}

\seealso{
\code{\link{as.character.biom}},
\code{\link{as.matrix.biom}},
\code{\link{dim.biom}},
\code{\link{is.biom}},
\code{\link{print.biom}}
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
