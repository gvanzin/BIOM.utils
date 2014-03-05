library(BIOM.utils)
library(MGRASTer)

########################################################
## matrix constructor (by hand)
########################################################
mm <- matrix(sample(1:100,1000,repl=T),nrow=250)
bb <- biom(mm, type="OTU table")
str(bb)
as.character(bb)
as.matrix(bb)
as.matrix(bb,alt.names=TRUE)		# error
as.matrix(bb,force.dense=TRUE)
as.matrix(bb,alt.names=TRUE,force.dense=TRUE)
dim(bb)
length(bb)
len(bb)
dimnames(bb)
is.biom(bb,fix=FALSE,check.all=FALSE,quiet=TRUE)
is.biom(bb,fix=FALSE,check.all=FALSE,quiet=FALSE)
is.biom(bb,fix=FALSE,check.all=TRUE,quiet=TRUE)
is.biom(bb,fix=FALSE,check.all=TRUE,quiet=FALSE)
tmp <- is.biom(bb,fix=TRUE,check.all=TRUE,quiet=TRUE)
tmp <- is.biom(bb,fix=TRUE,check.all=TRUE,quiet=FALSE)
print(bb)
summary(bb)

########################################################
## list constructor (by hand)
########################################################
mm <- matrix(sample(1:100,1000,repl=T),nrow=250)
ll <- list(
	type="OTU table",
	rows=rep(list(c(id="", metadata="")),nrow(mm)),
	columns=rep(list(c(id="", metadata="")),ncol(mm)),
	data=apply(mm, 1, as.list))			# as.list or list??
bb <- biom(ll)

########################################################
## list constructor (downloaded with MGRASTer)
########################################################
# if(require(MGRASTer)) {
obj <- call.MGRAST('ma', 'or', id=c(4447943.3, 4447192.3, 4447102.3, 4447103.3), 
	gro='family', so='Ref', resu='ab', ev=15, debug=TRUE)
str(obj)
bb <- biom(obj)
str(bb)
as.character(bb)
as.matrix(bb)
as.matrix(bb,alt.names=TRUE)
as.matrix(bb,force.dense=TRUE)
as.matrix(bb,alt.names=TRUE,force.dense=TRUE)
dim(bb)
length(bb)
len(bb)
dimnames(bb)
is.biom(bb,fix=FALSE,check.all=FALSE,quiet=TRUE)
is.biom(bb,fix=FALSE,check.all=FALSE,quiet=FALSE)
is.biom(bb,fix=FALSE,check.all=TRUE,quiet=TRUE)
is.biom(bb,fix=FALSE,check.all=TRUE,quiet=FALSE)
tmp <- is.biom(bb,fix=TRUE,check.all=TRUE,quiet=TRUE)
tmp <- is.biom(bb,fix=TRUE,check.all=TRUE,quiet=FALSE)
print(bb)
summary(bb)
# }

########################################################
## character constructor (downloaded with MGRASTer)
########################################################
# if(require(MGRASTer)) {
obj <- call.MGRAST('ma', 'or', id=c(4447943.3, 4447192.3, 4447102.3, 4447103.3), 
	gro='family', so='Ref', resu='ab', ev=15, parse=FALSE, debug=TRUE)
str(obj)
bb <- biom(obj)
str(bb)
as.character(bb)
as.matrix(bb)
as.matrix(bb,alt.names=TRUE)
as.matrix(bb,force.dense=TRUE)
as.matrix(bb,alt.names=TRUE,force.dense=TRUE)
dim(bb)
length(bb)
len(bb)
dimnames(bb)
is.biom(bb,fix=FALSE,check.all=FALSE,quiet=TRUE)
is.biom(bb,fix=FALSE,check.all=FALSE,quiet=FALSE)
is.biom(bb,fix=FALSE,check.all=TRUE,quiet=TRUE)
is.biom(bb,fix=FALSE,check.all=TRUE,quiet=FALSE)
tmp <- is.biom(bb,fix=TRUE,check.all=TRUE,quiet=TRUE)
tmp <- is.biom(bb,fix=TRUE,check.all=TRUE,quiet=FALSE)
print(bb)
summary(bb)
# }

########################################################
## check validation, all variants
########################################################
is.biom(bb,fix=FALSE,check.all=FALSE,quiet=TRUE)
is.biom(bb,fix=FALSE,check.all=FALSE,quiet=FALSE)
is.biom(bb,fix=FALSE,check.all=TRUE,quiet=TRUE)
is.biom(bb,fix=FALSE,check.all=TRUE,quiet=FALSE)
cc <- is.biom(bb,fix=TRUE,check.all=TRUE,quiet=TRUE)
cc <- is.biom(bb,fix=TRUE,check.all=TRUE,quiet=FALSE)

# convert-back-convert (both directions)
# fix a broken object
