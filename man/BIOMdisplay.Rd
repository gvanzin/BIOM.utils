\name{BIOM Display}
\alias{str.biom}
\alias{summary.biom}
\alias{print.biomsummary}
\alias{print.biom}
\alias{head.biom}
\alias{tail.biom}

\title{
Display BIOM data in full or part
}

\description{
Nicely print or summarize an object of class \code{biom}.  A summary omits printing the data table.
}

\usage{
\method{str}{biom}(object, ...)

\method{summary}{biom}(object, ...)

\method{print}{biom}(x, ...)

\method{head}{biom}(x, n=5, p=n, ...)

\method{tail}{biom}(x, n=5, p=n, ...)
}

\arguments{
  \item{x}{an object (\code{biom})}
  \item{object}{an object (\code{biom})}
  \item{n}{number of rows (single \code{integer})}
  \item{p}{number of columns (single \code{integer})}
  \item{...}{further arguments to default method (\code{str()} only)}
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

\code{print()} and \code{summary()} show the contents of the object, 
with the difference that the latter omits printing the BIOM data table.
The former always prints it in expanded (non-sparse) form.

\code{str()} shows the raw R structure of its argument
using the default \code{str()} with pleasing options.

\code{head()} and \code{tail()} return the data table's 
initial or final few rows and columns.
}

\value{
A \code{matrix} for \code{head}() and \code{tail()}.  See note below.

For the others, nothing useful.
}

\note{
When the object is \code{"sparse"} its data table is stored as
a three-column sparse representation matrix.
(See reference for details, and note that indices begin at zero.)
Such data is displayed by \code{print()} in an expanded (non-sparse) form,
but \code{head()} and \code{tail()} return 
parts of the sparse representation unexpanded.
}

\seealso{
\code{\link{biom}},
\code{\link{dim.biom}}
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
xx <- biom (dmat, quiet=TRUE)
yy <- biom (li4)

summary (xx)
print (xx)
head (xx)
tail (xx)
tail (xx, n=10, p=3)

tail (yy)
tail (yy, n=15)

##  biom class is just a list:
str (yy)
}
