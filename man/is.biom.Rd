\name{is.biom}
\alias{is.biom}

\title{
Check and repair representation of BIOM data
}

\description{
Check whether an object formally conforms to the \code{biom} class.
Optionally, attempt to repair it.
}

\usage{
is.biom(x, fix=FALSE, check.all=fix, quiet=!check.all)}

\arguments{
  \item{x}{an object (\code{biom})}
  \item{fix}{attempt repair? (\code{logical})}
  \item{check.all}{conduct thorough check? (\code{logical})}
  \item{quiet}{print messages? (\code{logical})}
}

\details{
In genomics, Biological Observation Matrix (BIOM) is a simple set of annotations
for a data table expressed in JSON.  BIOM can be described as a format, standard,
or even data structure.  Familiarity with BIOM is assumed, here.  For handling BIOM
data in R, there is the formal class \code{biom}.

This functions checks whether an object in R is of class \code{biom}.  

A few shortcuts are taken when \code{check.all=FALSE}, but otherwise the object is 
thoroughly validated.   A good result can not be guaranteed by setting \code{fix=TRUE},
and data may be lost, but small deviations from BIOM format can be repaired.
}

\value{
An error is signaled if \code{fix=TRUE} and the object can't be repaired.
Otherwise, if \code{fix=TRUE}, a \code{biom} object.
Otherwise, \code{TRUE} or \code{FALSE}.
}

\seealso{
\code{\link{biom}}
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

