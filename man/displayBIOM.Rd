\name{displayBIOM}
\alias{displayBIOM}
\alias{str.biom}
\alias{summary.biom}
\alias{print.biomsummary}
\alias{print.biom}

\title{
Display BIOM data in full or part
}

\description{
Nicely print or summarize an object of class "biom".  A summary omits printing the data table.
}

\usage{
\method{str}{biom}(object, ...)
\method{summary}{biom}(object, ...)
\method{print}{biom}(x, ...)
}

\arguments{
  \item{x}{an object ("biom')}
  \item{object}{an object ("biom")}
  \item{...}{further arguments to default method ("str" only)}
}

\details{
In genomics, Biological Observation Matrix (BIOM) is a simple set of annotations
for a data table expressed in JSON.  BIOM can be described as a format, standard,
or even data structure.  Familiarity with BIOM is assumed, here.  For handling BIOM
data in R, there is the formal class "biom".

These functions apply to an R object that is already of class "biom".

"print.biom" and "summary.biom" show the contents of the object, 
with the difference that the latter omits printing the BIOM data table.  
"str.biom" shows the formal structure of the object, simply calling the default
"str" method with pleasing options applied.
}

\value{
Nothing useful.
}

\seealso{
"\link{biom}",
"\link{str}",
"\link{summary}"
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


