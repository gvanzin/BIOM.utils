#  possible content for docs:
#
#  as() functions:
#  Conversion functions return x$data in sparse or dense representation, according to matrix_type.
#
#  as.matrix():
#  "dense=FALSE" has no effect when "matrix_type==\"dense\"".
#  when "matrix_type==\"sparse\"", the effect of "dense=TRUE" is to expand the sparse representation.
#  We do not employ the Matrix package, which implements sparse matrices in R.
#  But see below for an example using the "sparseMatrix" class from that package.
#
#  dim():
#  if matrix_type=="sparse" the return value has an attribute "nnz" (for "number not zero").  
#  Without "dense=TRUE", as.matrix() returns a matrix with this many rows and three columns,
#  comprising the sparse representation of the data.
#
#  as.character():
#  The argument "file" is not provided as part of a broader scheme to output files of arbitrary types.
#  Rather, it is provided because we regard JSON text _in_a_file_ as a native form of storage for BIOM
#  for csv/tsv, see below for an example with write.table()
#  #  how to write out data as a CSV or TSV file
#  write.table(as.matrix(x))
#  a "biom" object reflects its BIOM matrix_element_type implicitly, by its R storage mode.
#
#  biom.character():
#  The argument "file" is not provided as part of a broader scheme to input files of arbitrary types.
#  Rather, it is provided because we regard JSON text _in_a_file_ as a native form of storage for BIOM.
#  see below for an example of reading in a table in tsv, csv, or other text format
#  #  how to read in data from a CSV or TSV table
#  biom(as.matrix(read.table("file.txt")))
#  ... arguments are passed to fromJSON.  in particular to allow various character encodings.
#  a "biom" object reflects its BIOM matrix_element_type implicitly, by its R storage mode.
#
#  biom.matrix():
#  In R, values that seem to be integers are often not.
#  To force "matrix_element_type == \"int\"" in a "biom" object, use mode(xx) <- 'integer' before "biom(xx)"



#
#  BIOM format comprises a simple standard for annotation of a two-dimensional
#  matrix:  http://biom-format.org/documentation/format_versions/biom-1.0.html.
#
#  Here, a corresponding "biom" class is implemented to emphasize integration of
#  data and metadata with R functionality, plus convenient import and export.
#  The objects have class(x) == c("biom", "list").  
#
#  The representation is:
#
#		BIOM element				"biom" class element
#       ------------				--------------------
#		id							id
#		format						format
#		format_url					 --
#		type						type
#		generated_by				generated_by
#		date						date
#  
#		rows						 --
#			id						rownames (data)		OR		sparse $ dimnames [[1]]
#			metadata				rows
#		columns						 --
#			id						colnames (data) 	OR		sparse $ dimnames [[2]]
#			metadata				columns
#  
#		matrix_type					sparse
#		matrix_element_type			 --
#		shape						sparse $ dim
#		data						data
#
#		comment						comment
#
#  Notes:
#
#		1) x$data is always "matrix" class
#
#		2) if the BIOM matrix_type is "dense" then 
#				dim(x$data)			is		BIOM shape
#				rownames(x$data)	has		BIOM row ids  (but dimnames is unnamed list)
#				colnames(x$data)	has		BIOM column ids  (but dimnames is unnamed list)
#
#		3) whereas if matrix_type is "sparse" then
#				dim(x$data)"		is 		(# nonzero entries), 3
#				dimnames(x$data)	is		NULL
#				x$sparse$dim		is		BIOM shape
#				x$sparse$dimnames	has		BIOM row and column ids (list of "rows=", "columns=")
#

##############################################################################
##############################################################################
##
##  BASIC CLASS UTILITIES
##
##############################################################################
##############################################################################

#-----------------------------------------------------------------------------
#  str() method to pretty-print object structure
#-----------------------------------------------------------------------------

str.biom <- function (object, ...) {
	object <- unclass (object)
	str (object [c ("rows", "columns")], vec.len=1, nchar.max=35, list.len=3, 
		max.lev=3, no.list=T, give.attr=F)
	jj <- c("data", 
		if (!is.null (object$sparse)) "sparse", 
		"type", 
		"format", 
		"date", 
		"id", 
		"generated_by", 
		if (!is.null (object$comment)) "comment")
	str (object [jj], vec.len=1, nchar.max=35, no.list=T, give.attr=F)
	}

#-----------------------------------------------------------------------------
#  summary() method to nicely output meta-content (omitting the 'primary' data)
#-----------------------------------------------------------------------------

summary.biom <- function (object, ...) { 
	dd <- dim (object)
	ss <- paste (dd, collapse="x")
	if (!is.null (attr (dd, "nnz")))
		ss <- paste0 (ss, " of which ", attr (dd, "nnz"), " nonzero")
	xx <- with (object, { paste0(
			"[id:] ", id, "\n",

			"[generated_by:] ", generated_by, " on [date:] ", date, "\n",

			"[type:] ", if (exists ("sparse", inherits=F)) "sparse " else "dense ", 
			type, " (", ss, ")\n", 

			"[format:] ", format, "\n",
			
			if (exists ("comment", inherits=F)) paste("[comment:] ", comment, "\n")) })

	class (xx) <- c("biomsummary", "character")
	xx
	}

#-----------------------------------------------------------------------------
#  method to print the summary() object (also, returns it invisibly)
#-----------------------------------------------------------------------------

print.biomsummary <- function (x, ...) {
	cat (x)
	invisible (x)
	}

#-----------------------------------------------------------------------------
#  print() method to output full contents (canonically returns object invisibly)
#-----------------------------------------------------------------------------

print.biom <- function (x, ...) {
	print (as.matrix (x, dense=TRUE))
	cat("\n")
	print (summary (x))
	invisible (x)
	}

#-----------------------------------------------------------------------------
#  dim(x) method returns the BIOM "shape"
#  with additional attribute "nnz" (number not zero) if matrix_type is sparse.
#  "nnz" equals the number of values in the sparse representation.
#-----------------------------------------------------------------------------

dim.biom <- function (x) {
	with (x, {
		if (exists ("sparse", inherits=FALSE)) {
			attr (sparse$dim, "nnz") <- nrow (data)
			sparse$dim
		} else
			dim (data)
		} )
	}

#-----------------------------------------------------------------------------
#  dimnames() method returns BIOM "ids" in a list of 
#  two components named "rows" and "columns".
#  if the matrix is dense, those names are added.
#  but sparse$dimnames should already have those names, if it exists.
#
#  rownames() and colnames() are not generic, so not implemented here.
#-----------------------------------------------------------------------------

dimnames.biom <- function (x) {
	with (x, {
		if (exists ("sparse", inherits=FALSE)) {
			sparse$dimnames
		} else {
			names (dimnames (data)) <- c ("rows", "columns")
			dimnames (data)
		} } )
	}

#-----------------------------------------------------------------------------
#  "metadata() returns BIOM "metadata" in a list of
#  two component named "rows" and "columns".
#-----------------------------------------------------------------------------

metadata <- function (x, ...) UseMethod("metadata")

metadata.biom <- function (x, ...) {
	invisible (with (x, 
		list(
			rows = rows,
			columns = columns)))
	}

##############################################################################
##############################################################################
##
##  CONVERSION FROM
##
##############################################################################
##############################################################################

#-----------------------------------------------------------------------------
#  as.matrix() method returns:
#
#    for "dense", a "matrix" of the object's data
#
#    for "sparse", by default the 3-column sparse representation "matrix"
#      ...in that case "rownames" and "colnames" attributes are attached, containing 
#      the BIOM "id"s  ("dimnames" has a special meaning to R, so is not used)
#    but on request, the full "matrix" is returned, zero-filled (dims equal to the BIOM "shape")
#
#  the default value of "dense" is not "FALSE" to avoid misleading:
#  a dense matrix can only be returned dense
#-----------------------------------------------------------------------------

as.matrix.biom <- function (x, dense=NULL, ...) {
	invisible (with (x, {
		if (exists ("sparse", inherits=F)) {
			if (isTRUE (dense)) {

				data <- make.dense (data, sparse$dim)
				dimnames (data) <- sparse$dimnames
				names (dimnames (data)) <- NULL

			} else {
				attr (data, "rownames") <- sparse$dimnames [[1]]
				attr (data, "colnames") <- sparse$dimnames [[2]]
				}
			}

		data } ))
	}

#-----------------------------------------------------------------------------
#  as.list() method reorganizes internally to resemble the BIOM specification,
#  except that:
#		row and column "id"s are list elements, not part of "rows" and "columns"
#		"data" remains a matrix, not a list
#		"format_url" is missing
#-----------------------------------------------------------------------------

as.list.biom <- function (x, ...) {
	y <- list()

	y [c ("id", "format", "type", "generated_by", "date")] <- 
		x [c ("id", "format", "type", "generated_by", "date")]

	y$rows <- x$rows
	y$columns <- x$columns
	y$row.ids <- dimnames (x) [[1]]
	y$column.ids <- dimnames (x) [[2]]

	y$matrix_type			<- if (is.null (x$sparse)) "dense" else "sparse"	
	y$matrix_element_type	<- 
		if (is.integer(x$data)) {
			"int"
		} else if (is.numeric (x$data)) {
			"float"
		} else "unicode"
	y$shape					<- as.integer (dim (x))				# clear "nnz" attribute if present
	y$data					<- x$data

	y$comment <- x$comment
	invisible (y)
	}

#-----------------------------------------------------------------------------
#  as.character() converts to JSON text, in a "character" mode object, or file.
#  if, to file, then the text is prettily-formatted, otherwise not.
#-----------------------------------------------------------------------------

as.character.biom <- function (x, ..., file=NULL) {
	library(RJSONIO)

	x <- as.list (x)

	within (x, {
		format <- if (is.integer (data)) {
			"int"
		} else if (is.numeric (data)) {
			"float"
		} else {
			data <- as.character (data)
			"unicode"
			}
		data <- make.list (data)
		rows <- mapply (list,
			id = as.list(row.ids), 
			metadata = rows, 
			SIMPLIFY = F)
		columns <- mapply (list, 
			id = as.list(column.ids), 
			metadata = columns,
			SIMPLIFY=F) 
		format_url <- biom_format_url
		} )
	x$row.ids <- x$column.ids <- NULL

	if (!is.null (file)) {
		if (x$format == "unicode") {
			writeLines (enc2utf8 (toJSON (x, pretty=TRUE, ...)), file, useBytes=T)
		} else
			writeLines (toJSON (x, pretty=TRUE, ...), file)
		file
	} else
		invisible (toJSON (x, ...))
	}


##############################################################################
##############################################################################
##
##  CONSTRUCTORS
##
##############################################################################
##############################################################################

biom <- function (x, ...) UseMethod("biom")

#-----------------------------------------------------------------------------
#  here we require fromJSON() to return an appropriate list, 
#  and then call the "list" method on it.
#-----------------------------------------------------------------------------

biom.character <- function (x, ..., file=NULL, quiet=FALSE) {
	library (RJSONIO)

#  there is some work to be done here, to use RJSONIO more intelligently

	if (is.null (file)) {
		biom (fromJSON (x, asText=TRUE, simplify=TRUE, ...), quiet=quiet)
	} else
		biom (fromJSON (x, simplify=TRUE, ...), quiet=quiet)
	}

#-----------------------------------------------------------------------------
#  from "matrix", just invent something appropriate for all fields.
#  "sparse" may be given in two formats:
#		c (integer, integer)		indicating matrix dimensions
#		list (character, character)	indicating matrix rownames and colnames
#  this constructor does not handle:
#		metadata
#		matrix_element_type issues
#		comment
#-----------------------------------------------------------------------------

biom.matrix <- function (x, type=biom_table_types, sparse=NULL, ..., quiet=FALSE) {
	if (quiet) warning <- function (...) { }

	if (missing (type))
		warning ("assigning arbitrary type")

	if (is.null (sparse) && ncol(x) == 3)
		warning ("not interpreting three-column data as a sparse representation")

	y <- list()

	if (is.null (sparse)) {

		if (is.null (rownames (x))) {
			rownames (x) <- 1:nrow(x)
		} else if (anyDuplicated (rownames (x)))
			stop ("non-unique rownames")

		if (is.null (colnames (x))) {
			colnames (x) <- 1:ncol(x)
		} else if (anyDuplicated (colnames (x)))
			stop ("non-unique colnames")

		names (dimnames (x)) <- NULL

		y$rows <- replicate (nrow(x), character(0))
		y$columns <- replicate (ncol(x), character(0))

	} else {
		dimnames (x) <- NULL

		y$sparse <- if (is.list (sparse)) {
				names(sparse) <- c("rows", "columns")
				list(
					dimnames = sparse,
					dim = c (
						length (sparse[[1]]), 
						length (sparse[[2]])))
			} else if (is.numeric (sparse)) {
				list(
					dim = sparse,
					dimnames = list(
						rows = 1:sparse[1], 
						columns = 1:sparse[2]))
			} else
				stop ("bad specification of sparse data")

		y$rows <- replicate (y$sparse$dim[1], character(0))
		y$columns <- replicate (y$sparse$dim[2], character(0))
		}		

	y$data			<-		x
	y$type			<-		match.arg (type)

	y$id			<-		""
	y$generated_by	<-		paste("BIOM.utils (", packageVersion("BIOM.utils"), ")", sep="")
	y$date			<-		strftime(Sys.time())
	y$format		<- 		biom_format

	class (y) <- c ("biom", "list")
	invisible (y)
	}

#-----------------------------------------------------------------------------
#  construct using "matrix" method, then augment with any other provided data
#-----------------------------------------------------------------------------

biom.list <- function (x, ..., quiet=FALSE) { 
	if (quiet) warning <- function (...) { }

	if (is.null (x$data)) {
		mm <- matrix (nr=0, nc=0)
	} else if (is.matrix (x$data)) {
		mm <- x$data
	} else {
		mm <- simplify2array (x$data)
		if (is.vector (mm)) {				#  whoops, one-column matrix oversimplified to vector
			mm <- as.matrix (mm)
		} else
			mm <- t(mm)
		}

	if (!is.null (x$matrix_element_type))
		mm <- switch (x$matrix_element_type,
			int = as.integer,
			float = as.numeric,
			unicode = as.character) (mm)

	row.ids <- column.ids <- NULL
	if (!is.null (x$rows)) {
		row.ids <- sapply (x$rows, `[[`, "id")
		if (any (sapply (row.ids, is.null))) {
			row.ids <- NULL
		} else 
			x$rows <- lapply (x$rows, `[[`, "metadata")
		}

	if (!is.null (x$columns)) {
		column.ids <- sapply (x$columns, `[[`, "id")
		if (any (sapply (column.ids, is.null))) {
			column.ids <- NULL
		} else
			x$columns <- lapply (x$columns, `[[`, "metadata")
		}

	sparse <- NULL
	if (isTRUE (x$matrix_type == "sparse")) {
		sparse <- list (row.ids, column.ids)
		if (is.null (sparse [[1]]))
			sparse [[1]] <- 1:x$shape[1]
		if (is.null (sparse [[2]]))
			sparse [[2]] <- 1:x$shape[2]
	} else {
		if (!is.null (row.ids))
			rownames (mm) <- row.ids
		if (!is.null (column.ids))
			colnames (mm) <- column.ids
		if (is.null (rownames (mm)))
			rownames (mm) <- 1:nrow(mm)
		if (is.null (colnames (mm)))
			colnames (mm) <- 1:ncol(mm)
		}

# the following is a roundabout way of allowing "type" to be missing in the call

	ll <- list (x=mm, sparse=sparse, quiet=quiet)
	ll$type <- x$type
	y <- do.call (biom, ll)

	if (!is.null (x$rows)) 				y$rows 			<- x$rows
	if (!is.null (x$columns)) 			y$columns 		<- x$columns

	if (!is.null (x$id)) 				y$id 			<- x$id
	if (!is.null (x$generated_by)) 		y$generated_by 	<- x$generated_by
	if (!is.null (x$date)) 				y$date 			<- x$date
	y$comment <- x$comment
	
	invisible (y)
	}


##############################################################################
##############################################################################
##
##  PACKAGE UTILITIES
##
##############################################################################
##############################################################################

#-----------------------------------------------------------------------------
#  This routine builds the package example data:
#
#  BIOM.utils/data/examples.rda:
#		jtxt -- complete JSON text for BIOM object
#		smat -- sparse matrix of data, represented in three columns
#		dmat -- dense matrix, with row and column names
#		li1 -- minimal list, with data given as dense matrix with dimnames
#		li2 -- short list, data as above, but also with explicit row and column records
#		li3 -- short list as above, but with data formatted as list of rows
#		li4 -- complete list of BIOM components
#
#  BIOM.utils/inst/extdata/example-file.txt:
#		JSON text for a biom object
#
#-----------------------------------------------------------------------------

buildBiomExamples <- function(outfile.Rda="examples.rda", outfile.txt="example-file.txt") {
	library (RJSONIO)
	library (MGRASTer)
	triple <- function (x) paste(x, x, x, sep="")

	jtxt <- iconv(
		readLines(
			call.MGRAST (issue=FALSE, 'ma', 'or', id=c(4447943.3, 4447192.3, 4447102.3, 4447103.3), 
				gro='family', so='Ref', resu='ab', ev=15, quiet=TRUE),
			warn=FALSE),
		to="ASCII",
		sub="?")
	writeLines(jtxt, "example-file.txt")
	message ("Built ", outfile.txt, " in: ", getwd(), ".  For package build, move to BIOM.utils/inst/extdata")

	dmat <- matrix(101:200, nrow=20, dimnames=list(letters[1:20], LETTERS[1:5]))
	li1 <- list(
		data=dmat,
		type="OTU")
	li2 <- list(
		data = dmat,
		type = "OTU table",
		rows = lapply(triple(rownames(dmat)),
			function (x) c(id=x, metadata=paste("metadata of", x))),
		columns = lapply(triple(colnames(dmat)),
			function (x) c(id=x, metadata=paste("metadata of", x))))
	li3 <- list(
		data=lapply(apply(unname(dmat), 1, list), `[[`, 1),
		type="OTU table",
		rows=li2$rows,
		columns=li2$columns)
	li4 <- fromJSON (jtxt)
	li4 [c("matrix_element_value", "url")] <- NULL
	smat <- t(simplify2array(li4$data))

	save(smat, dmat, li1, li2, li3, li4, jtxt, file=outfile.Rda)
	message ("Built ", outfile.Rda, " in: ", getwd(), ".  For package build, move to BIOM.utils/data")
	}

#-----------------------------------------------------------------------------
#  routine simply to apply all biom functions to an object.
#  for testing.
#-----------------------------------------------------------------------------

applyBiomMethods <- function (x) {
	str(x)
	as.character(x)
	as.matrix(x)
	dim(x)
	dimnames(x)
	print(x)
	summary(x)
	is.biom(x)
	is.biom(x,fix=TRUE)
	}

#-----------------------------------------------------------------------------
#  return the example file of JSON text.  biom.character() may be applied to it.
#-----------------------------------------------------------------------------

exampleBiomFile <- function () {
	file.path (path.package ("BIOM.utils"), "extdata", "example-file.txt")
	}


##############################################################################
##############################################################################
##
##  LITTLE STUFF  (unexported)
##
##############################################################################
##############################################################################


#-----------------------------------------------------------------------------  
#  dense matrix to sparse
#-----------------------------------------------------------------------------  

dense2sparse <- function (x) {

	ss <- which (x != 0, arr.ind=TRUE)
	y <- t (mapply (
		function (i, j, m) c (i, j, m [i,j]),
		ss [,1],
		ss [,2],
		MoreArgs = list (x)))
	y
	}

#-----------------------------------------------------------------------------  
#  sparse matrix to dense.  allowed to specify dimensions for result.
#-----------------------------------------------------------------------------  

sparse2dense <- function (x, dim = c(max(x[,1]), max(x[,2]))) {

	colnames (x) <- c("row", "col", "value")
	df <- reshape (data.frame(x), v.names="value", idvar="row", timevar="col", direction="wide")
	cols <- paste ("value", 1:dim[2], sep=".")
	j <- cols %in% names(df)

	y <- matrix (NA, dim[1], dim[2])
	y [df$row, j] <- as.matrix (df [ , cols[j]])
	y [is.na (y)] <- 0
	y
	}

#-----------------------------------------------------------------------------  
#  matrix to list of its rows (or columns).   a limited inverse of simplify2array().
#-----------------------------------------------------------------------------  

matrix2list <- function (x, margin = 1) {

	f <- if (margin == 1) {
		function (i, x) x[i,] 
	} else
		function (j, x) x[,j]
	li <- as.list (
		if (margin == 1) {
			1:nrow(x)
		} else 
			1:ncol(x))

	lapply (li, f, x)
	}

#-----------------------------------------------------------------------------
#  we give warnings in a slightly idiosyncratic way
#-----------------------------------------------------------------------------

warning <- function (...) base::warning ("BIOM.utils: ", ...,  call.=FALSE)

#-----------------------------------------------------------------------------
#  concatenate things with spaces between
#-----------------------------------------------------------------------------

collapse <- function (x) paste (x, collapse=" ", sep="")

#-----------------------------------------------------------------------------
#  to report github commit at startup, preprocess this source with:
#    sed s/XXXBUILDXXX/$commit/g matR/R/init.R > init.Rtemp
#    mv init.Rtemp matR/R/init.R
#-----------------------------------------------------------------------------

.onAttach <- function (libname, pkgname) { 
	if ("package:biom" %in% search())
		warning ("conflicting package \"biom\" is already loaded")
	ss <- " build XXXBUILDXXX"
	if (substr (ss, 8, 15) == "XXXBUILD") ss <- ""
	packageStartupMessage(pkgname, " (", packageVersion(pkgname), ss, ")")
	}
