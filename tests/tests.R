DONE:  matrix, character, file
TO DO:  
* fixing  (maybe)

#  use RJSONIO in more sophisticated way .... or rather, replace with JSONlite

#  review is.biom() comprehensively yet again...
#  can use biom.matrix() within is.biom and/or biom.list() provided recursion is avoided
#  immediately apply simplify2array within is.biom().  require efficiency; neither do, nor code, things twice

#  remove rows() and columns() entirely?  yes I think so

#  replace with metadata() returning list of 2?  make generic so matR can have metadata.character
#  or rather, this seems ... silly

#  revise dumb / inaccurate warning messages
				

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
#  smat -- sparse data as matrix (no dimnames)
#  dmat -- dense matrix (dimnames)
#  li1 -- list:  dense matrix (with dimnames), type
#  li2 -- list:  dense matrix (with dimnames), type, rows, columns
#  li3 -- list:  dense rowlist, type, rows, columns
#  li4 -- list:  sparse data as list, and all other components
#  exfi -- list of files of:  jtxt; dmat saved as .rda; dmat written as tsv
#-----------------------------------------------------------------------------

library(BIOM.utils)

str(jtxt, list.len=5)
str(smat, list.len=5)
str(dmat, list.len=5)
str(li1, list.len=5)
str(li2, list.len=5)
str(li3, list.len=5)
str(li4, list.len=5)
str(exfi)

#-----------------------------------------------------------------------------
#  constructing from nothing
#-----------------------------------------------------------------------------

biom()

#-----------------------------------------------------------------------------
#  constructing from matrix
#-----------------------------------------------------------------------------

#  from named matrix
biom (dmat)
biom (dmat, quiet=TRUE)

#  from nameless matrix
biom (unname (dmat))
biom (unname (dmat), quiet=TRUE)

#  from named matrix; type specified
biom (dmat, type="Taxon")
biom (dmat, type="Taxon", quiet=TRUE)

#  from nameless matrix; sparse representation not recognized
biom (smat, type="Taxon")
biom (smat, type="Taxon", quiet=TRUE)

#  from nameless matrix; sparse representation specified
biom (smat, type="Taxon", matrix_type="sparse")
biom (smat, type="Taxon", matrix_type="sparse", quiet=TRUE)

#  from named matrix; ensure matrix_element_type is "int"
mm <- dmat
mode(mm) <- "integer"
biom(mm, type="Taxon", matrix_type="dense")

#  from named matrix; ensure matrix_element_type is "float"
mm <- dmat
mode(mm) <- "double"
biom(mm, type="Taxon", matrix_type="dense")

#  from named matrix; ensure matrix_element_type is "Unicode"
mm <- dmat
mode(mm) <- "character"
biom(mm, type="Taxon", matrix_type="dense")

#  from nameless matrix; sparse specified; ensure matrix_element_type is "int"
mm <- smat
mode(mm) <- "integer"
biom(mm, type="Taxon", matrix_type="sparse")

#  from nameless matrix; sparse specified; ensure matrix_element_type is "float"
mm <- smat
mode(mm) <- "double"
biom(mm, type="Taxon", matrix_type="sparse")

#  from named matrix; ensure matrix_element_type is "Unicode"
mm <- smat
mode(mm) <- "character"
biom(mm, type="Taxon", matrix_type="sparse")


#-----------------------------------------------------------------------------
#  constructing from character (JSON text)
#-----------------------------------------------------------------------------

biom (jtxt)
biom (jtxt, quiet=TRUE)

#-----------------------------------------------------------------------------
#  constructing from name of file  (confirm: reliably calls character method?)
#-----------------------------------------------------------------------------

#  from an .rda file, read in a single object of any kind, and apply biom()
biom (file=exfi$rda)

#  from a non-rda file, the contents of which isValidJSON() approves
biom (file=exfi$json)

#  from a non-rda file _supposed_ to be JSON
biom (file=exfi$json, force.JSON=TRUE)

#  from a file, not isValidJSON() nor .rda, but rather a text data table
biom (file=exfi$text)


#-----------------------------------------------------------------------------
#  constructing from list
#-----------------------------------------------------------------------------

#  an empty object
biom (list())
biom (list(), quiet=TRUE)

#  from list of:  named matrix (dense)
biom (list(
	data=dmat))
biom (quiet=TRUE, list(
	data=dmat))

#  from list of:  nameless matrix (dense)
biom (list(
	data=unname(dmat)))
biom (quiet=TRUE, list(
	data=unname(dmat)))

# from example list:  named matrix (dense), biom type
biom (li1)
biom (li1, quiet=TRUE)

#  from list of:  named matrix (dense), biom type
biom (list(
	data=dmat, 
	type="Taxon"))
biom (quiet=TRUE, list(
	data=dmat, 
	type="Taxon"))

#  from example list:  unnamed matrix (dense), biom type
li <- li1
li$data <- unname(li$data)
biom(li)
biom(li, quiet=TRUE)

#  from list of:  named matrix (dense), annotations but no metadata
biom (list(
	data=dmat, 
	type="Taxon", 
	matrix_type="dense",
	id="my first biom matrix",
	generated_by="science"))
biom (quiet=TRUE, list(
	data=dmat, 
	type="Taxon", 
	matrix_type="dense",
	id="my first biom matrix",
	generated_by="science"))

#  from list of:  unnamed matrix, sparse indicated
biom (list(
	data=smat,
	matrix_type="sparse"))
biom (quiet=TRUE, list(
	data=smat,
	matrix_type="sparse"))

#  from list of:  named matrix (dense), biom type, metadata (supercedes dimnames)
biom (li2)
biom (li2, quiet=TRUE)

#  from list of:  rowlist (dense)
biom (li3 ["data"])
biom (li3 ["data"], quiet=TRUE)

#  from list of:  rowlist (dense), biom type, metadata (supercedes any names from data)
biom (li3)
biom (li3, quiet=TRUE)

#  from list of:  rowlist (dense), biom type, partial metadata
li <- li3
li$rows <- NULL
biom(li)									# some issue
biom(li, quiet=TRUE)

#  from list of:  entrylist (sparse), with everything
biom (li4)
biom (li4, quiet=TRUE)

#  from list of:  matrix (sparse), with everything
li <- li4
li$data <- as.matrix (biom (li))
biom (li4)

#  from list of:  matrix (dense), with everything (metadata supercedes dimnames)
li <- li4
li$data <- as.matrix (biom (li), force.dense=TRUE)
dimnames (li$data) <- list (1:nrow(li$data), 1:ncol(li$data))
li$matrix_type <- "dense"
biom (li)

#  same as above, but omit to reset matrix_type  (so, an error)
li <- li4
li$data <- as.matrix (biom (li), force.dense=TRUE)
try (biom (li))

#  from list of:  rowlist (dense), with everything
li <- li4
li$data <- as.matrix (biom (li), force.dense=TRUE)
li$data <- lapply (apply (dmat, 1, list), unlist, rec=F)
li$matrix_type <- "dense"
biom (li)


#-----------------------------------------------------------------------------
#  validation and fixing
#-----------------------------------------------------------------------------

#  fail:  not "list"
try(is.biom(dmat, fix=TRUE))

#  fail:  missing names
li <- li4
names(li) <- NULL
try(is.biom(li, fix=TRUE))

#  fail:  missing "data"
li <- li4
li$rows <- NULL
try(is.biom(li, fix=TRUE))

#  succeed:  missing non-required component added
li <- li4
li$generated_by <- NULL
is.biom(li, fix=TRUE)

#  succeed:  missing "matrix_type"
li <- li4
li$matrix_type <- NULL
is.biom(li, fix=TRUE)						# some issue

#  succeed:  missing "biom" class added
is.biom(li4, fix=TRUE)
is.biom(is.biom(li4, fix=TRUE))

#  succeed:  extraneous field(s) dropped
li <- li4
li$foo <- "baz"
is.biom(li, fix=TRUE)

#  succeed:  wrong "shape" fixed
li <- li4
li$shape <- c(10,5)
is.biom(li, fix=TRUE)						# some issue


#-----------------------------------------------------------------------------
#  robustness of conversion
#
#  1 - return to original type via biom
#  2 - convert to another type via biom
#  3 - convert to biom via another type via biom
#-----------------------------------------------------------------------------

tt <- tempfile()
as.character (biom (jtxt), file=tt)
as.character (biom (jtxt))
as.matrix (biom (jtxt))
as.matrix (biom (jtxt), force.dense=TRUE)
as.list (biom (jtxt))
biom (as.matrix (biom (jtxt)))
biom (as.matrix (biom (jtxt), force.dense=TRUE))
biom (as.list (biom (jtxt)))
unlink (tt)

as.matrix (biom (smat, matrix="sparse"))
as.matrix (biom (smat, matrix="sparse"), force.dense=TRUE)
as.character (biom (smat, matrix="sparse"))
as.list (biom (smat, matrix="sparse"))
biom (as.character (biom (smat, matrix="sparse")))

as.matrix (biom (dmat))
as.character (biom (dmat))
as.list (biom (dmat))
biom (as.character (biom (dmat)))
biom (as.list (biom (dmat)))

as.list (biom (li1)))
as.matrix (biom (li1))
as.character (biom (li))
biom (as.matrix (biom (li1)))
biom (as.character (biom (li)))

as.list (biom (li4)))
as.matrix (biom (li4))
as.matrix (biom (li4), force.dense=TRUE)
as.character (biom (l4))
biom (as.matrix (biom (li4)), force.dense=TRUE)
biom (as.character (biom (l4)))
