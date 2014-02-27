yy <- RBIOM(call.MGRAST(...))
summary(yy)
dim(yy)
length(yy)


## if a RBIOM object gives trouble, get more info with:
## is.BIOM(obj, check.all=TRUE)
##
## and maybe try:
## fixed.obj <- is.RBIOM(obj, fix=TRUE)
##
