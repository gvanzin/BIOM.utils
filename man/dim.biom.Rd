\name{dim.biom}
\alias{dim.biom}
\alias{len}
\alias{dimnames.biom}

\title{Size attributes of data table of BIOM object}
\description{
Report dimensions, number of non-zero elements, and row and column names 
of the data table stored in an object of class \code{biom}.}

\usage{
\method{dim}{biom}(x)
len(x)
\method{dimnames}{biom}(x)}

\arguments{
  \item{x}{an object (\code{biom})}}

\details{
The data table of a BIOM object can be stored densely or sparsely.
\code{dim} gives the dimensions of the data table that \code{x} contains.
\code{len} gives the number of data elements actually stored in \code{x}.
If \code{x$sparse=TRUE}, this should be the number of non-zero elements in the table.

BIOM format requires the fields \code{id} and \code{metadata} per row and per column.
Only the \code{id} fields can be extracted with \code{dimnames}.
To access the \code{metadata} fields, see \code{as.matrix.biom}.}

\value{
For \code{dim}, the number of rows and columns, a (length-two) integer vector.
For \code{len}, a single integer.
For \code{dimnames}, a list of two character vectors containing, respectively, row and column names.}

\author{
Daniel T. Braithwaite}

\seealso{
\code{\link{biom}}, \code{\link{as.matrix.biom}}}

\examples{#}

