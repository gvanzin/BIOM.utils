\name{BIOM Constants}
\alias{biom_format}
\alias{biom_format_url}
\alias{biom_fields}
\alias{biom_table_types}
\alias{biom_matrix_types}
\alias{biom_matrix_element_types}

\docType{data}

\title{
Constants related to BIOM format
}

\description{
These constants enumerate components of BIOM format and
their valid values (controlled vocabularies).
}

\usage{
biom_format
biom_format_url
biom_fields
biom_table_types
biom_matrix_element_types
biom_matrix_types
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

\code{biom_format} gives the latest BIOM version implemented by this package.  

\code{biom_format_url} gives its associated URL.

\code{biom_fields} lists the components specified by BIOM.

Each other constant enumerates the allowed values of its corresponding BIOM component.
}

\references{
\href{http://wwww.biom-format.org}{BIOM format} \cr
\href{http://wwww.json.org}{JSON} \cr
}

\seealso{
\code{\link{biom}}
}

\examples{
biom_format_url [biom_format]
biom_fields
biom_table_types
biom_matrix_element_types
biom_matrix_types
}

\keyword{datasets}
