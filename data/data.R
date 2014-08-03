#-----------------------------------------------------------------------------
#  constants of BIOM format
#-----------------------------------------------------------------------------
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
biom_element_types <- c(
	"int",
	"float",
	"unicode")
biom_matrix_types <- c(
	"dense",
	"sparse")