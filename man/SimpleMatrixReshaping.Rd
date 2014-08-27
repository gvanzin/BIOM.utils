\name{Matrix Manipulations for BIOM Format}
\alias{sparse2dense}
\alias{dense2sparse}
\alias{matrix2list}

\title{
Convert matrix data between dense, sparse, and list representations
}

\description{
Convert two-dimensional data between ordinary (dense) matrix type,
sparse representation, and a list of rows (or columns).
}

\usage{
	sparse2dense(x, dim=NULL)
	dense2sparse(x)
	matrix2list(x)
}

\arguments{
  \item{x}{object to convert (\code{matrix})}
  \item{dim}{number of rows and columns (length-two \code{integer})}
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

The S3 class code{biom} and its methods facilitate analyses
by expressing BIOM data as objects in the R environment.
These manipulations of two-dimensional (matrix) data are
sometimes handy when working with BIOM data.  
The sparse format used by BIOM is conventional, 
but see the reference for exact details.

All \code{names()} and \code{dimnames()} are removed.
}

\value{
\code{sparse2dense()} and \code{dense2sparse()} return a \code{matrix}.

\code{matrix2list()} returns a list of the rows of \code{x}.
}

\note{
Like R these functions begin indices at 1, whereas BIOM counts from 0, 
so adjust if necessary.
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
xx <- smat
xx[,1:2] <- xx[,1:2] + 1
sparse2dense (xx)
sparse2dense (xx, c(266,10))

matrix2list (dmat)
matrix2list (t(dmat))
}