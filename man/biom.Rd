\name{biom}
\alias{biom}
\alias{biom.matrix}
\alias{biom.list}
\alias{biom.character}

\title{Construct BIOM object}
\description{
Construct an object of class \code{biom} from given data, supplementing with standard values as necessary.}

\usage{
biom(x, ...)
\method{biom}{list}(x, ...)
\method{biom}{matrix}(x, type=c("OTU table","Pathway table","Function table","Ortholog table",
	"Gene table","Metabolite table","Taxon table"), ...)
\method{biom}{character}(x, ...)}

\arguments{
  \item{x}{an object of appropriate class, convertible to \code{biom}}
  \item{type}{The object type, according to BIOM format}
  \item{\dots}{unused}}

\details{
In genomics, the Biological Observation Matrix (BIOM) format is a JSON-based standard for storing a data table with metadata.
In R, the \code{biom} class is simply a list that correctly mirrors the structure required by the BIOM format.
These constructor functions are provided to create \code{biom} objects.

When \code{x} is a \code{matrix}, the parameter \code{type} is expected, 
but other fields required by the BIOM format are automatically assigned.
When \code{x} is a \code{list}, its class is set to \code{biom} and then \code{is.biom} is called with \code{fix=TRUE}.
(So in that case, \code{x} should already contain most required BIOM fields.)
When \code{x} is \code{character}, it should be JSON text for a properly formed BIOM object.}

\value{
An object of class \code{biom}, except in case of error.}

\references{
\href{http://metagenomics.anl.gov}{MG-RAST metagenome annotation server},
\href{http://wwww.biom-format.org}{BIOM format}, 
\href{http://wwww.json.org}{JSON}}

\author{
Daniel T. Braithwaite}

\seealso{
\code{\link{as.character.biom}},
\code{\link{as.matrix.biom}},
\code{\link{dim.biom}},
\code{\link{is.biom}},
\code{\link{print.biom}}}

\examples{#}

