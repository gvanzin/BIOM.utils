Package BIOM.utils defines an S3 class and utilities in the R language
for conveniently handling the Biological Observation Matrix (BIOM) format.
See: http://biom-format.org
and: http://metagenomics.anl.gov

The primary functions made available are:

is.biom <- function (x, fix=FALSE, check.all=fix, quiet=!check.all)
biom.list <- function (x, quiet=FALSE, ...)
biom.matrix <- function (x, type = c("OTU table","Pathway table","Function table","Ortholog table", "Gene table","Metabolite table","Taxon table"), quiet=FALSE, ...)
biom.character <- function (x, ...)

as.matrix.biom <- function(x, rownamer=byid, colnamer=byid, force.dense=TRUE, ...)
as.character.biom <- function (x, ...)

dim.biom <- function(x)
dimnames.biom <- function(x)

print.biom <- function(x, ...)
summary.biom <- function(object, ...)
str.biom <- function(object, ...)
