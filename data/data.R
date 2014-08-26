#-----------------------------------------------------------------------------
#  the package's "data" comprises:
#
#  (1) constants of BIOM format, defined below:
#		biom_format
#		biom_format_url
#		biom_fields
#		biom_table_types
#		biom_matrix_element_types
#		biom_matrix_types
#
#  (2) these objects saved in .rda file:
#		jtxt	complete JSON text for a BIOM object
#		smat	sparse data as matrix (no dimnames)
#		dmat	dense matrix (dimnames)
#		li1		list:  dense matrix (with dimnames), type
#		li2		list:  dense matrix (with dimnames), type, rows, columns
#		li3		list:  dense rowlist, type, rows, columns
#		li4		list:  sparse data as list, and all other components
#
#  (3) the file path of a sample BIOM JSON text file, returned by:
#		exampleBiomFile()		(inst/extdata/example-file.txt in package source)
#
#-----------------------------------------------------------------------------

biom_format <- "Biological Observation Matrix 1.0"
biom_format_url <- "http://biom-format.org/documentation/format_versions/biom-1.0.html"
names(biom_format_url) <- biom_format
biom_fields <- c(
	"rows",
	"columns",
	"data",
	"shape",
	"matrix_type",
	"matrix_element_type",
	"type",
	"format",
	"format_url",
	"date",
	"id",
	"generated_by")
biom_table_types <- c(
	"OTU table",
	"Pathway table",
	"Function table",
	"Ortholog table",
	"Gene table",
	"Metabolite table",
	"Taxon table")
biom_matrix_element_types <- c(
	"int",
	"float",
	"unicode")
biom_matrix_types <- c(
	"dense",
	"sparse")
