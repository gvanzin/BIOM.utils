#-----------------------------------------------------------------------------
#  the package's "data" comprises:
#
#  (1) constants of BIOM format:
#		biom_format
#		biom_format_url
#		biom_fields
#		biom_table_types
#		biom_matrix_element_types
#		biom_matrix_types
#  the package and users need these, so they are in sysdata.rda and exported via NAMESPACE
#
#  (2) these objects saved in .rda file:
#		jtxt	complete JSON text for a BIOM object
#		smat	sparse data as matrix (no dimnames)
#		dmat	dense matrix (dimnames)
#		li1		list:  dense matrix (with dimnames), type
#		li2		list:  dense matrix (with dimnames), type, rows, columns
#		li3		list:  dense rowlist, type, rows, columns
#		li4		list:  sparse data as list, and all other components
#  these are for users and the package does not need them, so they are in data/examples.rda and exported
#
#  (3) 	a file of sample JSON text: inst/extdata/example-file.txt
# 		function exampleBiomFile(), exported, which returns its name in any particular installation
#
#-----------------------------------------------------------------------------
