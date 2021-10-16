
# Author: tim
###############################################################################

# transform mortality rates mx to lifetable death distribution dx
mx2dx <- function(mx){
	ax <- c(.1,rep(.5,110))
	qx <- lt_id_ma_q(nMx=mx, nax=ax, AgeInt=rep(1,111), closeout = TRUE,IMR=NA)
	lt_id_l_d(lt_id_q_l(qx,radix=1))
}
# transform mortality rates mx to lifetable standard deviation sd
mx2sd <- function(mx){
	dx  <- mx2dx(mx)
	vx  <- momentN(dx, n = 2, ax = c(.1,rep(.5,110)))
	sdx <- suppressWarnings(sqrt(vx))
	sdx
}
# transform mortality rates mx to lifetable average years of life lost to death
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
# transform mortality rates mx to lifetable remaining life expectancy ex
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
# transform mortality rates mx to lifetable remaining life expectancy ex deleting for a specific cause of death
mx2ex_i <- function(mx,mxc){
	# Following Preston Box 4.2, except w simplified ax 
	# this is approach c from the book
	# reference table functions
	ax  <- c(.1,rep(.5,110))
	qx  <- lt_id_ma_q(nMx=mx, nax=ax, AgeInt=rep(1,111), closeout = TRUE,IMR=NA)
	px  <- 1-qx
	
	# power perturb
	# TR: note, N years later, risk of dividing by 0
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
# some functions used in 06_Decomposition.R


# weighted mean of x where w are weights
wmean <- function(x,w){
	sum(x*w)/sum(w)
}

# quick sd from a cause specific matrix of mx where age is rows and causes are columns
mx2sd_mat <- function(mxmat){
	mx2sd(rowSums(mxmat))
}

# quick sd from a cause specific vector of mx resulting from having taken vec of a cause specific matrix
# of the same with age in rows and causes in columns
mx2sd_vec <- function(mxcvec,n=3,Age=10){
	dim(mxcvec) <- c(length(mxcvec)/3,3)
	mx2sd_mat(mxcvec)[Age+1]
}

# custom function to operate on a chunk of GBD processed data, this
# takes the vec of the causes from the given population and the low
# mortality reference population and does a horiuchi decomp
decomp_sd <- function(.SD,Age=10){
	pars1 <- as.matrix(.SD[,c("Mo","Mh","Mw")])
	pars2 <- as.matrix(.SD[,c("low_Mo","low_Mh","low_Mw")])
	
	
	
	comp <- horiuchi(mx2sd_vec,c(pars1),c(pars2),N=20,Age=Age)
	dim(comp)      <- dim(pars1)
	dimnames(comp) <- dimnames(pars1)
	data.table(data.frame(Age=0:110,comp))
}

# wash wrinse repeat for edagger:

# edagger from an age-cause matrix
mx2edagger_mat <- function(mxmat){
	mx2edagger(rowSums(mxmat))
}

# edagger from an age-cause vec of an age cause matrix
mx2edagger_vec <- function(mxcvec,n=3,Age=10){
	dim(mxcvec) <- c(length(mxcvec)/3,3)
	mx2edagger_mat(mxcvec)[Age+1]
}

# decompose it from a chunk of processed GBD data
decomp_edagger <- function(.SD,Age=10){
	pars1 <- as.matrix(.SD[,c("Mo","Mh","Mw")])
	pars2 <- as.matrix(.SD[,c("low_Mo","low_Mh","low_Mw")])
	
	comp <- horiuchi(mx2edagger_vec,c(pars1),c(pars2),N=20,Age=Age)
	dim(comp)      <- dim(pars1)
	dimnames(comp) <- dimnames(pars1)
	data.table(data.frame(Age=0:110,comp))
}

###
# Functions used in DataPrep_GBD for graduation:


# use this for all-cause mortality graduation.
GBD.pclm <- function(.SD, omega = 110, mort = "a"){
  
  # determine which variables to use
  Dx     <- paste0("D",mort)
  Mx     <- paste0("M",mort)
  
  # siphon off vectors to use, throw out IMR
  x      <- .SD$age[-1]
  y      <- .SD[[Dx]][-1]
  m      <- .SD[[Mx]][-1]
  
  n      <- nrow(.SD)
  
  if (all(is.na(m))){
    return(rep(NA,111))
  }
  # just retain IMR, no problem
  m0     <- .SD[[Mx]][1]
  
  if (any(is.na(y)) | any(is.na(m))){
    rmind <- is.na(y) | is.na(x)
  }
  
  
  # how wide open interval, for practical purposes
  nlast  <- omega - max(x) + 1
  
  # back out exposure
  offset <- y / m
  
  # split exposure using pclm...
  off1   <- pclm(x = x, y = offset, nlast = nlast, control = list(lambda = 1 / 1e6))$fitted
  
  # possibly deflate counts & exposures for splitting- this was breaking
  # in some very large populations. Odd.
  fac    <- ifelse(sum(y) > 2e6,10,1)
  
  # split counts using split exposure as offset returns rates
  M      <- pclm(x = x, 
                 y = y / fac, 
                 nlast = nlast, 
                 offset = off1 / fac, 
                 control = list(lambda = 1))$fitted
  M[is.nan(M)] <- 0
  
  # append to saved IMR
  M      <- c(m0, M)  
  M
}

# use this for homicide and 'other violence' graduation
GBD.mono.ms <- function(.SD, omega=110,lambda=.1,mort="h"){
  
  # determine which variables to use
  Dx     <- paste0("D",mort)
  Mx     <- paste0("M",mort)
  
  # siphen off vectors
  x      <- .SD$age
  y      <- .SD[[Dx]]
  m      <- .SD[[Mx]]
  
  # backstop in case no counts
  if (all(is.na(m))){
    return(rep(NA,111))
  }
  
  if (all(m == 0)){
    return(rep(0,111))
  }
  
  
  # don't touch infant mort. Derivative too high, messes up splines
  # of all kinds. Until Carl Schmertmann boxes up his nifty solution.
  m0     <- .SD[[Mx]][1]
  
  # over how many ages do we want to distribute the open age group?
  # Here no one lives beyond 110
  nlast  <- omega - max(x) + 1
  
  # back out exposure from rates & counts. Sometimes it's hideous.
  offset <- y / m
  
  # split it to single ages using monotonic spline over cumulative exposure.
  # it's fast and sufficient, since we only use this as an offset.
  # TR: this way we let nlast do the work
  off1   <- graduate_mono(offset, 
                          AgeInt = age2int(Age = x, OAvalue = nlast), 
                          Age = x,
                          OAG = FALSE)[-1] 
  off1[off1<0] <- 0
  
  # spread rates over single ages within intervals
  m5     <- rep(m, times = age2int(Age = x, OAvalue = nlast))
  
  # assign 0 weights to pathological cases
  w      <- rep(1,110)
  ind    <- off1 == 0 | is.na(off1) | is.infinite(off1)
  w[ind] <- 0
  
  # first step, 'blocky' hypothetical counts that we'll smooth
  d      <- m5[-1] * off1
  d[ind] <- 0
  
  # if rates are too low things break ¯\_(ツ)_/¯
  # so we keep the 'blocky' rates
  if (floor(sum(d)) < 10){
    return(m5)
  } 
  
  # But there are some cases where we can make the smoother 
  # work by inflating counts+rates. So the trick is to inflate,
  # then smooth, then deflate by sae factor
  if (sum(d) < 100){
    fac <- 100
  } else {
    fac <- 1
  }
  d      <- d * fac
  
  # The smoother function. Sometimes it complains even though
  # it finds an acceptible solution.
  mod    <- suppressWarnings(Mort1Dsmooth(
    x = 1:110, 
    y = d, 
    offset = log(off1), 
    w = w, 
    lambda = lambda, # gives as arg
    method = 3))
  
  # append smoothed rates (deflated) to saved infant mort
  mx     <- c(m0, exp(mod$logmortality) / fac)
  mx
}

# combines the prior two, operating on and returning a chunk
GBD.chunk <- function(.SD,omega=110){
  
  # graduate the 3 mortality vectors.
  # I really wish I knew of a way to do a compositional graduation
  # of all 3 at once...
  M  <- suppressWarnings(GBD.pclm(.SD, omega = 110,mort = "a"))
  Mh <- GBD.mono.ms(.SD, omega = 110, mort = "h")
  Mw <- GBD.mono.ms(.SD, omega = 110, mort = "w")
  # Mh + Mw are here not guaranteed to be < M,
  # so will have to check for that. Unlikely as
  # they are most often orders of magnitude different
  
  data.table(data.frame(
    ISO3 = rep(.SD$ISO3[1],111),
    location = rep(.SD$location[1], 111),
    year = rep(.SD$year[1], 111),
    sex = rep(.SD$sex[1], 111),
    age = 0:110,
    Ma = M, Mh = Mh, Mw = Mw))
}





# deprecated test code.
# faster calculation steps for taking an age cause vector and calculating edagger from it for a truncated age range.
# meaning that years lost are also truncated! Be careful to see if this is used in practice
# edaggTemp <- function(mxcvec,.N=3,a=10,n=60){
# 	dim(mxcvec) <- c(111,.N)
# 	mx          <- rowSums(mxcvec)
# 	n           <- n + 1
# 	mx          <- mx[-(1:a)][1:n]
# 	ax          <- rep(.5,n)
# 	qx          <- mx / (1 + (1 - ax) * mx) 
# 	lx          <- cumprod(c(1, 1 - qx[-n]))
# 	Lx          <- (lx[-n] + lx[-1])/2
# 	Tx          <- lt_id_L_T(Lx)
# 	ex          <- Tx / lx[-n]
# 	dxx         <- -diff(lx)
# 	wmean(ex,dxx)
# }
# 
# 
# decomp_edagger_temp <- function(.SD,Age=10,n=60){
# 	pars1 <- as.matrix(.SD[,c("Mo","Mh","Mw")])
# 	pars2 <- as.matrix(.SD[,c("low_Mo","low_Mh","low_Mw")])
# 	
# 	comp <- horiuchi(edaggTemp,c(pars1),c(pars2),N=20,a=Age,n=n)
# 	dim(comp)      <- dim(pars1)
# 	dimnames(comp) <- dimnames(pars1)
# 	data.table(data.frame(Age=0:110,comp))
# }