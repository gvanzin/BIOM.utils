\name{BIOM Import}
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

\method{biom}{character}(x, ..., file=NULL, quiet=FALSE)

\method{biom}{matrix}(x, type=biom_table_types, sparse=NULL, ..., quiet=FALSE)

\method{biom}{list}(x, ..., quiet=FALSE)
}

\arguments{
  \item{x}{an object convertible to \code{biom}}
  \item{type}{the type of BIOM table to be constructed (string)}
  \item{sparse}{\code{dim}s or \code{dimnames} when sparse (\code{integer} or \code{list}, length-two)}
  \item{\dots}{arguments to \code{fromJSON()}}
  \item{file}{file containing JSON text for BIOM data (string)}
  \item{quiet}{print messages and warnings? (\code{logical})}
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
Each function above transforms an object of a basic R type into class \code{biom}.

The \code{character} method is suitable for importing BIOM data properly speaking, 
that is, JSON text conforming to the BIOM specification.  
The data is read from \code{file}, when provided.
Arguments from \code{\dots} are passed to \code{fromJSON()} 
in order to allow for different character encodings, 

To make a \code{biom} object from a TSV or CSV file, see examples below.
Of course, it is possible to bring \code{biom} objects 
in and out of the session with \code{save()} and \code{load()}.

The \code{matrix} method accepts an ordinary (\code{"dense"}) matrix
or a three-column sparse matrix representation 
(using indices starting at zero, per BIOM).
If the latter, \code{sparse} should also be provided, 
as a length-two \code{integer} giving the data table's \code{shape},
or a length-two list giving its row and column \code{id}s.
It may also be simply \code{TRUE}, 
in which case the smallest possible \code{shape} is assigned.

The \code{list} method accepts a \code{list} of components allowed or required by BIOM, 
inventing something reasonable for missing components, with certain qualifications:

\code{shape} is required if \code{matrix_type} is specified as \code{"sparse"};
\code{data} may be given as a \code{list} of rows, 
a \code{list} of triples sparsely representing a matrix, or a \code{matrix}; 
in the last case, its \code{dimnames()} may be used
to provide row and column \code{id}s;
but \code{ids} from \code{rows} and \code{columns},
if provided, supercede \code{dimnames()}.
}

\value{
An object of class \code{biom}, invisibly.
}

\note{
A \code{biom} object reflects its \code{matrix_element_type} implicitly
with the \code{storage.mode()} of its \code{data}.
Accordingly, applying \code{storage.mode<-()} prior to \code{biom()} can be useful.
See examples below.

Also, note that values seeming to be integers in R often are not.
}

\seealso{
\code{\link{as.character.biom}},
\code{\link{as.matrix.biom}},
\code{\link{dim.biom}},
\code{\link{print.biom}},
\code{\link{storage.mode}}
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

##  two ways to the same result:
ff <- exampleBiomFile()
txt <- readLines (ff)
biom (txt)
biom (file=ff)

##  choose what fields to include with a list:
biom (list (data=smat, matrix_type="sparse", shape=c(266,4),
	matrix_element_type="unicode", comment="no comment"), quiet=TRUE)

xx <- matrix2list (cbind (LETTERS[1:20], paste ("some metadata for row", 1:20)))
xx <- lapply (xx, "names<-", c("id", "metadata"))
biom (list (data=dmat, type="Gene table", rows=xx, id="1234567890"), quiet=TRUE)

##  the same result in two ways, again:
write.table (dmat, file=tt, sep=",")
biom (dmat, "Function table")
biom (as.matrix (read.table (file=tt, sep=",")), "Func")

##  all the same:
biom (smat, sparse=TRUE, quiet=TRUE)
biom (smat, sparse=c(266,4), quiet=TRUE)
biom (smat, sparse=list (paste ("row", 1:266), paste ("column", 1:4)), quiet=TRUE)

##  enforce matrix_element_type to be "int":
mm <- dmat
storage.mode (mm) <- "integer"
biom (mm, quiet=TRUE)

unlink (tt)
}
