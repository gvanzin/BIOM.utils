################################################################################
## These routines are made available to users:
##
## biom
## biom.character
## biom.list
## biom.matrix
##
## as.character.biom
## as.matrix.biom
##
## dim.biom
## dimnames.biom
## len
##
## str.biom
## print.biom
## summary.biom
##
## is.biom
##
################################################################################

library(BIOM.utils)

################################################################################
## prepackaged example inputs
##
## jtxt -- complete JSON text for BIOM object
## smat -- sparse matrix of data, represented in three columns
## dmat -- dense matrix, with row and column names
## li1 -- minimal list, with data given as dense matrix with dimnames
## li2 -- short list, data as above, but also with explicit row and column records
## li3 -- short list as above, but with data formatted as list of rows
## li4 -- complete list of BIOM components
################################################################################

load("examples.Rda")
str(smat, list.len=5)
str(dmat, list.len=5)
str(li1, list.len=5)
str(li2, list.len=5)
str(li3, list.len=5)
str(li4, list.len=5)
str(jtxt, list.len=5)

################################################################################
## show simple constructions
################################################################################

biom(smat, type="OTU"); biom(dmat, type="OTU")

--> some quirky behavior here:
biom(li1); biom(li2); biom(li3); biom(li4)

biom(jtxt)

## confirm all methods work on them

biom.test(biom(smat, type="OTU")); biom.test(biom(dmat, type="OTU"))
biom.test(biom(li1)); biom.test(biom(li2)); biom.test(biom(li3)); biom.test(biom(li4))
biom.test(biom(jtxt))

################################################################################
## show variations that WORK
################################################################################


################################################################################
## show variations that DON'T WORK
################################################################################

## because valid "type" is required
try(biom(dmat))
try(biom(dmat, "foo"))

## matrix intended as sparse will be interpreted as dense
try(biom(smat, type="OTU"))

## because no row/column info when dimnames are removed
li <- li1
li$data <- unname(li$data)
try(biom(li))

## because required component "rows" is missing
li <- li3
li$rows <- NULL
try(biom(li))

########################################################
## show validation, including fixing
########################################################

## class is not "list" -- fails
try(is.biom(dmat, fix=TRUE))

## "list" is missing names -- fails
li <- li4
names(li) <- NULL
try(is.biom(li, fix=TRUE))

## "list" is missing a required component ("type","rows","columns","data") -- fails
li <- li4
li$rows <- NULL
try(is.biom(li, fix=TRUE)))

## "list" is missing a non-required component -- succeeds
li <- li4
li$generated_by <- NULL
is.biom(li, fix=TRUE)

####---> problem.  with character(0) ??
## missing "matrix_type" can be inferred -- succeeds
li <- li4
li$matrix_type <- NULL
is.biom(li, fix=TRUE)

## class is "list" not "biom" -- succeeds
is.biom(li4, fix=TRUE)
is.biom(is.biom(li4, fix=TRUE))

## "list" has extraneous field -- succeeds
li <- li4
li$foo <- "baz"
is.biom(li, fix=TRUE)

## incorrect "shape" can be fixed -- succeeds
li <- li4
li$shape <- c(10,5)
is.biom(li, fix=TRUE)


########################################################
## show robustness of conversion
########################################################

biom(as.character(biom(mm)))
biom(as.character(biom(jsontext)))
biom(as.character(biom(shortlist)))
biom(as.character(biom(longlist)))

biom(as.matrix(biom(mm)))
biom(as.matrix(biom(jsontext)))
biom(as.matrix(biom(shortlist)))
biom(as.matrix(biom(longlist)))
