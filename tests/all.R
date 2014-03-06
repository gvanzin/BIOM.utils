library(BIOM.utils)

mm <- matrix(sample(1:100,1000,repl=T),nrow=250)
ll1 <- list(
	type="OTU table",
	rows=rep(list(c(id="", metadata="")),nrow(mm)),
	columns=rep(list(c(id="", metadata="")),ncol(mm)),
	data=apply(mm, 1, as.list))
ll2 <- list(
	type="OTU table",
	rows=rep(list(c(id="", metadata="")),nrow(mm)),
	columns=rep(list(c(id="", metadata="")),ncol(mm)),
	data=apply(mm, 1, list))
if(require(MGRASTer)) {
	dl <- call.MGRAST('ma', 'or', id=c(4447943.3, 4447192.3, 4447102.3, 4447103.3), 
		gro='family', so='Ref', resu='ab', ev=15, parse=FALSE, debug=TRUE)
	dll <- call.MGRAST('ma', 'or', id=c(4447943.3, 4447192.3, 4447102.3, 4447103.3), 
		gro='family', so='Ref', resu='ab', ev=15, debug=TRUE)
}
str(mm, list.len=5)
str(ll1, list.len=5)
str(ll2, list.len=5)
str(dl, list.len=5)
str(dll, list.len=5)

########################################################
## matrix constructor (by hand)
########################################################
bb1 <- biom(mm, type="OTU table")
str(bb1, list.len=5)
as.character(bb1)
as.matrix(bb1)
as.matrix(bb1,alt.names=TRUE)		# error
as.matrix(bb1,force.dense=TRUE)
as.matrix(bb1,alt.names=TRUE,force.dense=TRUE)
dim(bb1)
length(bb1)
len(bb1)
dimnames(bb1)
print(bb1)
summary(bb1)

########################################################
## list constructor (by hand)
########################################################
bb2 <- biom(ll1)
str(bb2, list.len=5)
as.character(bb2)
as.matrix(bb2)
as.matrix(bb2,alt.names=TRUE)		# error
as.matrix(bb2,force.dense=TRUE)
as.matrix(bb2,alt.names=TRUE,force.dense=TRUE)
dim(bb2)
length(bb2)
len(bb2)
dimnames(bb2)
print(bb2)
summary(bb2)

########################################################
## list constructor (downloaded with MGRASTer)
########################################################
if(require(MGRASTer)) {
bb3 <- biom(dl)
str(bb3, list.len=5)
as.character(bb3)
as.matrix(bb3)
as.matrix(bb3,alt.names=TRUE)
as.matrix(bb3,force.dense=TRUE)
as.matrix(bb3,alt.names=TRUE,force.dense=TRUE)
dim(bb3)
length(bb3)
len(bb3)
dimnames(bb3)
print(bb3)
summary(bb3)
}

########################################################
## character constructor (downloaded with MGRASTer)
########################################################
if(require(MGRASTer)) {
bb4 <- biom(dll, list.len=5)
str(bb4)
as.character(bb4)
as.matrix(bb4)
as.matrix(bb4,alt.names=TRUE)
as.matrix(bb4,force.dense=TRUE)
as.matrix(bb4,alt.names=TRUE,force.dense=TRUE)
dim(bb4)
length(bb4)
len(bb4)
dimnames(bb4)
print(bb4)
summary(bb4)
}

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

as.character(biom(as.character(bb)))
biom(as.character(biom(tt)))
