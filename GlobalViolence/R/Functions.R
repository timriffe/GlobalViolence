
# Author: tim
###############################################################################

mx2dx <- function(mx){
	ax <- c(.1,rep(.5,110))
	qx <- mxax2qx(nMx=mx, nax=ax, AgeInt=rep(1,111), closeout = TRUE,IMR=NA)
	lx2dx(qx2lx(qx,radix=1))
}

mx2sd <- function(mx){
	dx  <- mx2dx(mx)
	vx  <- momentN(dx, n = 2, ax = c(.1,rep(.5,110)))
	sdx <- suppressWarnings(sqrt(vx))
	sdx
}

mx2edagger <- function(mx){
	ax <- c(.1,rep(.5,110))
	qx <- mxax2qx(nMx=mx, nax=ax, AgeInt=rep(1,111), closeout = TRUE,IMR=NA)
	lx <- qx2lx(qx,radix=1)
	dx <- lx2dx(lx)
	Lx <- lxdxax2Lx(lx = lx, ndx = dx, nax = ax, AgeInt=rep(1,111))
	Tx <- Lx2Tx(Lx)
	ex <- Tx / lx
	DX <- matrix(dx,nrow=111,ncol=111)
	DX[upper.tri(DX)] <- 0
	colSums(sweep(DX,2,colSums(DX),`/`) * ex)
}
mx2ex <- function(mx){
	ax <- c(.1,rep(.5,110))
	qx <- mxax2qx(nMx=mx, nax=ax, AgeInt=rep(1,111), closeout = TRUE,IMR=NA)
	lx <- qx2lx(qx)
	dx <- lx2dx(lx)
	Lx <- lxdxax2Lx(lx = lx, ndx = dx, nax = ax, AgeInt=rep(1,111))
	Tx <- Lx2Tx(Lx)
	Tx / lx
}



# what about cause deleted functions?
#X   <- GBD[location=="Rwanda" & Sex == 1 & year == 1994]
#mx  <- X$M
#mxc <- X$Mh
mx2ex_i <- function(mx,mxc){
	# Following Preston Box 4.2, except w simplified ax 
	# this is approach c from the book
	# reference table functions
	ax  <- c(.1,rep(.5,110))
	qx  <- mxax2qx(nMx=mx, nax=ax, AgeInt=rep(1,111), closeout = TRUE,IMR=NA)
	px  <- 1-qx
	
	# power perturb
	Ri  <- (mx - mxc)/mx
	pxi <- (px)^Ri
	
	# calculate per standard formulas
	lxi <- cumprod(c(1,pxi))[1:111]
	dxi <- lx2dx(lxi)
	Lxi <- lxdxax2Lx(lx = lxi, ndx = dxi, nax = ax, AgeInt=rep(1,111))
	Txi <- Lx2Tx(Lxi)
	Txi / lxi
}

#(mx2ex_i(mx,mxc) - mx2ex(mx) ) -
#(mx2ex(mx-mxc) -mx2ex(mx) )

