\name{BIOM Examples}
\alias{buildBiomExamples}
\alias{applyBiomMethods}
\alias{exampleBiomFile}
\alias{jtxt}
\alias{smat}
\alias{dmat}
\alias{li1}
\alias{li2}
\alias{li3}
\alias{li4}

\title{
BIOM examples and related utilities
}

\description{
Example objects for getting started, and related utilities.
}

\usage{
jtxt; smat; dmat; li1; li2; li3; li4
exampleBiomFile()
applyBiomMethods(x)
buildBiomExamples(rdafile="examples.rda", jsonfile="example-json.txt")
}

\arguments{
  \item{rdafile}{filename for .rda saves of example objects (string)}
  \item{jsonfile}{filename for saved example JSON text (string)}
  \item{x}{object (biom)}
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
Simple data objects of several kinds are provided to experiment 
with handling BIOM in R.
See the examples below.  

\code{buildBiomExamples()} draws from MG-RAST (see below) to construct
these objects and saves them in designated files.
(This utility is used to build the package.)

\code{exampleBiomFile()} returns the path to a file of JSON text 
correctly structured as BIOM.

\code{applyBiomMethods()} is a utility for testing that simply applies all 
\code{biom} methods to a given object.
}

\value{
Only \code{exampleBiomFile()} has a useful return value, as described above.
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
str(jtxt)
str(smat)
str(dmat)
str(li1)
str(li2)
str(li3)
exampleBiomFile()

xx <- biom (file=exampleBiomFile())
yy <- biom (dmat, quiet=TRUE)
zz <- biom (li4)

\dontrun{
##  this prints a large volume of text:
applyBiomMethods(xx)
applyBiomMethods (yy)

##  this requires package MGRASTer:
tt1 <- tempfile()
tt2 <- tempfile()
buildBiomExamples (tt1, tt2)
unlink (tt1)
unlink (tt2)
}
}
