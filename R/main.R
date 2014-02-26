######################################################################################
## S3 implementation of BIOM format as S3 with class equal to c(list, RBIOM)
## required components: id, format, format_url, type, generated_by, date, rows, columns, matrix_type, matrix_element_type, shape, data
## optional components: comment
## see: http://biom-format.org/documentation/format_versions/biom-1.0.html
## 
## package includes validation, conversion, and other utility methods
## small on purpose
######################################################################################


######################################################################################
## should actually print the whole friggin thing
######################################################################################
print.RBIOM <- function (x, ...) {
}


######################################################################################
## perhaps this is unnecessary?
######################################################################################cra
str.RBIOM <- function (object, ...) {}


######################################################################################
## validate and print summary
######################################################################################
summary.RBIOM <- function (object, ...) { 
#-------check fields in object
fields <- c("id","format","format_url","type","generated_by","date","rows",
	"columns","matrix_type","matrix_element_type","shape","data")
fields.present <- fields %in% names(object)
fields.extra <- names(object) %in% fields
if(!any(fields.present))
	warning("required field(s) missing: ", 
		paste(fields[!fields.present], collapse=" ", sep=""))
if(any(fields.extra))
	warning("contains unrecognized field(s): ", 
		paste(fields[!fields.present], collapse=" ", sep=""))
#-------check controlled vocabularies
if(! (object$type %in%
	c("OTU table","Pathway table","Function table","Ortholog table",
	"Gene table","Metabolite table","Taxon table")))
	warning("invalid \"type\": ", object$type)
if(! (object$matrix_type %in%
	c("sparse","dense")))
	warning("invalid \"matrix_type\": ", object$type)
if(! (object$matrix_element_type %in%
	c("int", "float", "unicode")))
	warning("invalid \"matrix_type\": ", object$type)
#-------check rows and columns
x <- !sapply(object$rows, function (row) metadata %in% names(row))
if(any(x))
	warning("row(s) missing \"metadata\": ", which(x))
x <- !sapply(object$rows, function (row) id %in% names(row))
if(any(x))
	warning("row(s) missing \"id\": ", which(x))
x <- !sapply(object$columns, function (col) metadata %in% names(col))
if(any(x))
	warning("column(s) missing \"metadata\": ", which(x))
x <- !sapply(object$columns, function (col) id %in% names(col))
if(any(x))
	warning("column(s) missing \"id\": ", which(x))

cat(x$matrix_type, " ", x$type,  " (", x$format, ") ", x$id, "\n", sep ="")
}


######################################################################################
## shape of matrix -- whether or not sparse
######################################################################################
dim.RBIOM <- function(x) as.integer(unlist(x$shape))


######################################################################################
## number of actually provided data elements -- different meaning for sparse and dense
######################################################################################
length.RBIOM <- function(x) {
	if(identical(x$type, "sparse"))
		length(x$data)
	else
		length(unlist(x$data))
}


######################################################################################
## data as matrix type with row and column names
######################################################################################
as.matrix.RBIOM <- function(x, ...) {
m <- matrix (unlist (from$data), ncol = 3, byrow = TRUE)
as.matrix (Matrix::sparseMatrix (i = 1 + m [,1], j = 1 + m [,2], x = m [,3]))
}


######################################################################################
## returns length-2 list of rownames, colnames
## ideal would be to define generics for those functions
## but it seems like asking for trouble, for now;
## ...whereas names() is already generic
######################################################################################
names.RBIOM <- function(x) {
nn <- list()
nn[[1]] <- as.character(try(sapply (x$rows, `[[`, i = "id")))
nn[[2]] <- as.character(try(sapply (x$columns, `[[`, i = "id")))
names(nn) <- c(row, column)
nn
}


######################################################################################
## convert to JSON text
######################################################################################
unlist.RBIOM <- function (x, ...) {}
as.character.RBIOM <- unlist.RBIOM


######################################################################################
## CONSTRUCTORS
######################################################################################
RBIOM <- function (x, ...) UseMethod(RBIOM, x)

######################################################################################
## construct from list -- assuming all necessary fields are present?
######################################################################################
RBIOM.list <- function (x, ...) { 
class(x) <- c(RBIOM, class(x))
x
}

######################################################################################
## construct from matrix, inventing something for all fields appropriately
######################################################################################
RBIOM.matrix <- function (x, ...) {
}

######################################################################################
## construct from JSON text
######################################################################################
RBIOM.character <- function (x, ...) {
fromJSON (x, asText = TRUE, simplify = TRUE)
}
