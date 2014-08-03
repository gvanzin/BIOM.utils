\name{BIOMconstants}
\alias{BIOMconstants}
\alias{biom_fields}
\alias{biom_table_types}
\alias{biom_matrix_types}
\alias{biom_element_types}

\docType{data}

\title{
Constants used in representing BIOM data
}

\description{
These objects enumerate the components of the BIOM representation as well as 
the valid values (controlled vocabularies), where applicable, of required components.
}

\usage{
biom_fields
biom_table_types
biom_matrix_types
biom_element_types
}

\details{
In genomics, Biological Observation Matrix (BIOM) is a simple set of annotations
for a data table expressed in JSON.  BIOM can be described as a format, standard,
or even data structure.  Familiarity with BIOM is assumed, here.  For handling BIOM
data in R, there is the class \code{biom}.

\code{biom_fields} lists the components of BIOM.  Each of the other three objects
documented here lists the valid values of the eponymous BIOM component.
}

\references{
\href{http://wwww.biom-format.org}{BIOM format}
}

\examples{
biom_fields
biom_table_types
biom_matrix_types
biom_element_types
}

\keyword{datasets}
