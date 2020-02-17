
# Author: tim
###############################################################################

mx2dx <- function(mx){
	ax <- c(.1,rep(.5,110))
	qx <- lt_id_ma_q(nMx=mx, nax=ax, AgeInt=rep(1,111), closeout = TRUE,IMR=NA)
	lt_id_l_d(lt_id_q_l(qx,radix=1))
}

mx2sd <- function(mx){
	dx  <- mx2dx(mx)
	vx  <- momentN(dx, n = 2, ax = c(.1,rep(.5,110)))
	sdx <- suppressWarnings(sqrt(vx))
	sdx
}

mx2edagger <- function(mx){
	ax <- c(.1,rep(.5,110))
	qx <- lt_id_ma_q(nMx=mx, nax=ax, AgeInt=rep(1,111), closeout = TRUE,IMR=NA)
	lx <- lt_id_q_l(qx,radix=1)
	dx <- lt_id_l_d(lx)
	Lx <- lt_id_lda_L(lx = lx, ndx = dx, nax = ax, AgeInt=rep(1,111))
	Tx <- lt_id_L_T(Lx)
	ex <- Tx / lx
	DX <- matrix(dx,nrow=111,ncol=111)
	DX[upper.tri(DX)] <- 0
	colSums(sweep(DX,2,colSums(DX),`/`) * ex)
}
mx2ex <- function(mx){
	ax <- c(.1,rep(.5,110))
	qx <- lt_id_ma_q(nMx=mx, nax=ax, AgeInt=rep(1,111), closeout = TRUE,IMR=NA)
	lx <- lt_id_q_l(qx)
	dx <- lt_id_l_d(lx)
	Lx <- lt_id_lda_L(lx = lx, ndx = dx, nax = ax, AgeInt=rep(1,111))
	Tx <- lt_id_L_T(Lx)
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
	qx  <- lt_id_ma_q(nMx=mx, nax=ax, AgeInt=rep(1,111), closeout = TRUE,IMR=NA)
	px  <- 1-qx
	
	# power perturb
	Ri  <- (mx - mxc)/mx
	pxi <- (px)^Ri
	
	# calculate per standard formulas
	lxi <- cumprod(c(1,pxi))[1:111]
	dxi <- lt_id_l_d(lxi)
	Lxi <- lt_id_lda_L(lx = lxi, ndx = dxi, nax = ax, AgeInt=rep(1,111))
	Txi <- lt_id_L_T(Lxi)
	Txi / lxi
}

#(mx2ex_i(mx,mxc) - mx2ex(mx) ) -
#(mx2ex(mx-mxc) -mx2ex(mx) )

# ----------------------------------
# some functions used in Comparison.R
wmean <- function(x,w){
	sum(x*w)/sum(w)
}

mx2sd_mat <- function(mxmat){
	mx2sd(rowSums(mxmat))
}

mx2sd_vec <- function(mxcvec,n=3,Age=10){
	dim(mxcvec) <- c(length(mxcvec)/3,3)
	mx2sd_mat(mxcvec)[Age+1]
}
decomp_sd <- function(.SD,Age=10){
	pars1 <- as.matrix(.SD[,c("Mo","Mh","Mw")])
	pars2 <- as.matrix(.SD[,c("low_Mo","low_Mh","low_Mw")])
	
	
	
	comp <- horiuchi(mx2sd_vec,c(pars1),c(pars2),N=20,Age=Age)
	dim(comp)      <- dim(pars1)
	dimnames(comp) <- dimnames(pars1)
	data.table(data.frame(Age=0:110,comp))
}


mx2edagger_mat <- function(mxmat){
	mx2edagger(rowSums(mxmat))
}

mx2edagger_vec <- function(mxcvec,n=3,Age=10){
	dim(mxcvec) <- c(length(mxcvec)/3,3)
	mx2edagger_mat(mxcvec)[Age+1]
}
decomp_edagger <- function(.SD,Age=10){
	pars1 <- as.matrix(.SD[,c("Mo","Mh","Mw")])
	pars2 <- as.matrix(.SD[,c("low_Mo","low_Mh","low_Mw")])
	
	comp <- horiuchi(mx2edagger_vec,c(pars1),c(pars2),N=20,Age=Age)
	dim(comp)      <- dim(pars1)
	dimnames(comp) <- dimnames(pars1)
	data.table(data.frame(Age=0:110,comp))
}

# fast temp
edaggTemp <- function(mxcvec,.N=3,a=10,n=60){
	dim(mxcvec) <- c(111,.N)
	mx          <- rowSums(mxcvec)
	n           <- n + 1
	mx          <- mx[-(1:a)][1:n]
	ax          <- rep(.5,n)
	qx          <- mx / (1 + (1 - ax) * mx) 
	lx          <- cumprod(c(1, 1 - qx[-n]))
	Lx          <- (lx[-n] + lx[-1])/2
	Tx          <- lt_id_L_T(Lx)
	ex          <- Tx / lx[-n]
	dxx         <- -diff(lx)
	wmean(ex,dxx)
}
decomp_edagger_temp <- function(.SD,Age=10,n=60){
	pars1 <- as.matrix(.SD[,c("Mo","Mh","Mw")])
	pars2 <- as.matrix(.SD[,c("low_Mo","low_Mh","low_Mw")])
	
	comp <- horiuchi(edaggTemp,c(pars1),c(pars2),N=20,a=Age,n=n)
	dim(comp)      <- dim(pars1)
	dimnames(comp) <- dimnames(pars1)
	data.table(data.frame(Age=0:110,comp))
}