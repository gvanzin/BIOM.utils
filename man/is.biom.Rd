\name{is.biom}
\alias{is.biom}

\title{
Check and repair representation of BIOM data
}

\description{
Check whether an object formally conforms to the "biom" class.
Optionally, attempt to repair it.
}

\usage{
is.biom(x, fix=FALSE, check.all=fix, quiet=!check.all)}

\arguments{
  \item{x}{an object ("biom")}
  \item{fix}{attempt repair? ("logical")}
  \item{check.all}{conduct thorough check? ("logical")}
  \item{quiet}{print messages? ("logical")}
}

\details{
In genomics, Biological Observation Matrix (BIOM) is a simple set of annotations
for a data table expressed in JSON.  BIOM can be described as a format, standard,
or even data structure.  Familiarity with BIOM is assumed, here.  For handling BIOM
data in R, there is the formal class "biom".

This functions checks whether an object in R is of class "biom".  

A few shortcuts are taken when "check.all" is "FALSE", but otherwise the object is 
thoroughly validated.   A good result can not be guaranteed by setting "fix" to "TRUE"
and data may be lost, but small deviations from BIOM format can be repaired.
}

\value{
An error is signaled if "fix" is "TRUE" and the object can't be repaired.
Otherwise, if "fix" is "TRUE", a "biom" object.
Otherwise, "TRUE" or "FALSE".
}

\seealso{
"\link{biom}"
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

