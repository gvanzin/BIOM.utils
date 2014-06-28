\name{BIOMexamples}
\alias{BIOMexamples}
\alias{buildBiomExamples}
\alias{applyBiomMethods}
\alias{exampleBiomFile}

\title{
BIOM examples and related utilities
}

\description{
Example objects for trying out methods of "biom" class, and related utilities.
}

\usage{
buildBiomExamples(outfile.Rda="examples.rda", outfile.txt="example-file.txt")
applyBiomMethods(x)
exampleBiomFile()
jtxt
smat
dmat
li1
li2
li3
li4
}

\arguments{
  \item{outfile.Rda}{filename for .rda saves of example objects (string)}
  \item{outfile.txt}{filename for saved example JSON text (string)}
  \item{x}{object (biom)}
}

\details{
In genomics, Biological Observation Matrix (BIOM) is a simple set of annotations
for a data table expressed in JSON.  BIOM can be described as a format, standard,
or even data structure.  Familiarity with BIOM is assumed, here.  For handling BIOM
data in R, there is the formal class "biom".

Simple data objects of several kinds are provided to experiment with handling BIOM in R.
See the examples below.  "buildBiomExamples" constructs these objects and saves them
in designated files.  (It is a utility used to build the package.)

"exampleBiomFile()" returns the full path, in the current installation, of a file of 
(correctly structured JSON) BIOM data.

"applyBiomMethods" is a testing utility that simply applies all BIOM methods
to a given object.
}

\value{
Only "exampleBiomFile" has a meaningful return value (as described).
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