#-----------------------------------------------------------------------------
#  exported routines and example data:
#
#  biom
#  biom.character
#  biom.list
#  biom.matrix
#  as.character.biom
#  as.matrix.biom
#  dim.biom
#  dimnames.biom
#  str.biom
#  print.biom
#  summary.biom
#  is.biom
#
#  jtxt -- complete JSON text for a BIOM object
#  smat -- three-column representation of a sparse matrix (no rownames/colnames)
#  dmat -- small (dense) matrix with rownames/colnames
#  li1 -- minimal list, with data given as dense matrix with dimnames
#  li2 -- short list, data as above, but also with explicit row and column records
#  li3 -- short list as above, but with data formatted as list of rows
#  li4 -- complete list of BIOM components
#-----------------------------------------------------------------------------

library(BIOM.utils)

str(smat, list.len=5)
str(dmat, list.len=5)
str(li1, list.len=5)
str(li2, list.len=5)
str(li3, list.len=5)
str(li4, list.len=5)
str(jtxt, list.len=5)
exf <- biomExampleFile()

#-----------------------------------------------------------------------------
# basic use
#-----------------------------------------------------------------------------

*** biom (jtxt)								# JSON text object
biom (jtxt, quiet=TRUE)
biom (file=exf)				# JSON text file
biom (file=exf, quiet=TRUE)

biom (dmat)									# matrix with row/colnames
biom (dmat, quiet=TRUE)
biom (unname (dmat))						# matrix without row/colnames
biom (unname (dmat), quiet=TRUE)
biom (dmat, type="Taxon")					# biom type specified
biom (dmat, type="Taxon", quiet=TRUE)

biom (smat)									# this way, sparse data is not recognized
biom (smat, quiet=TRUE)

*** biom (list())							# empty biom object
biom (list(), quiet=TRUE)
biom (list (data=dmat))						# list of only matrix
biom (list (data=dmat), quiet=TRUE)
biom (list (data=unname(dmat)))				# list of only matrix (without row/colnames)
biom (list (data=unname(dmat)), quiet=TRUE)
biom (list (								# list of matrix and biom type
	data=dmat, 
	type="Taxon"))
biom (list (								# list of matrix and biom type
	data=dmat, 
	type="Taxon"), quiet=TRUE)
biom (list (								# list of whatever you like
	data=dmat, 
	type="Taxon", 
	matrix_type="dense",
	id="my first biom matrix",
	generated_by="science"))
biom (list (								# list of whatever you like
	data=dmat, 
	type="Taxon", 
	matrix_type="dense",
	id="my first biom matrix",
	generated_by="science"), quiet=TRUE)
*** biom (list (data=smat, matrix_type="sparse"))		# sparse data should be recognized this way
biom (list (data=smat, matrix_type="sparse"), quiet=TRUE)

biom (li1)							# list has "data" (dense named matrix),"type"
biom (li1, quiet=TRUE)
*** biom (li2)						# list has "data" (dense named matrix),"type","rows","columns"
									# "id"s from "rows" and "columns" should supercede rownames/colnames of "data"
biom (li2, quiet=TRUE)
biom (li3)							# list has "data" (dense row list),"type","rows","columns"
biom (li3, quiet=TRUE)
biom (li4)							# list has "data" (sparse list), all other components
biom (li4, quiet=TRUE)

li <- li1
li$data <- unname(li$data)
biom(li)							# list has "data" (dense unnamed matrix),"type"
bimo(li, quiet=TRUE)

li <- li3
li$rows <- NULL
biom(li)							# list has "data" (dense row list),"type","rows","columns"
bimo(li, quiet=TRUE)



mode (mm) <- 'integer'				# force matrix_element_type == 'int'


#-----------------------------------------------------------------------------
#  validation and fixing
#-----------------------------------------------------------------------------

## class is not "list" -- fails
try(is.biom(dmat, fix=TRUE))

## "list" is missing names -- fails
li <- li4
names(li) <- NULL
try(is.biom(li, fix=TRUE))

## "list" is missing a required component ("type","rows","columns","data") -- fails
li <- li4
li$rows <- NULL
try(is.biom(li, fix=TRUE))

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


#-----------------------------------------------------------------------------
#  robustness of conversion
#-----------------------------------------------------------------------------

biom(as.character(biom(dmat)))
biom(as.character(biom(jtxt)))
biom(as.character(biom(li1)))
biom(as.character(biom(li4)))

biom(as.matrix(biom(dmat)))
biom(as.matrix(biom(jtxt)))
biom(as.matrix(biom(li1)))
biom(as.matrix(biom(li4)))
