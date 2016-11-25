# kmpar
# Parallel Gaussian process emulation of large data sets using DiceKriging.
# Doug McNeall dougmcneall@gmail.com


# We take two approaches to GP emulation of large data sets 
# 1) Direct (point-by-point) computation and 2) Dimension reduction.
# Either are amenable to parallelisation, as each element of (say)
# a map or each PC can be emulated independently. The basic approach
# is to break the data set into a list, each element of which is
# fit using a km model.

library(parallel)
library(DiceKriging)

emlist = function(X, Y){
  # Create a list of objects to be emulated
  # X             design matrix with (nrow) instances of (ncol) inputs
  # Y             matrix of outputs, with one row
  #               for each row of X
  
  d = ncol(Y)
  em.list = vector(mode='list', length=d)
  
  for(i in 1:d){
    em.obj = NULL
    em.obj$X = X
    em.obj$y = Y[, i]
    em.list[[i]] = em.obj
  }
  em.list
}

# !N.B. need to check that we're passing the formula in correctly.
km.wrap = function(form, em, ...){
  # wrapper function to fit km model to list em.
  # each list element in em contains a design matrix X 
  # and an output vector y
  out = NA
  fit = try(km(form, design=em$X, response=em$y, control=list(trace=FALSE), ...), silent=TRUE)
  #if(class(fit) == "try-error"){
  #  out = NA
  #}
  #else{
  #  out = fit 
  #}
  out = fit
  out
}


# -------------------------------------------------------------------
# Test case, modified from km examples in DiceKriging.
# -------------------------------------------------------------------

# a 16-points factorial design, and the corresponding response
d = 2; n = 16
design.fact = expand.grid(x1=seq(0,1,length=4), x2=seq(0,1,length=4))
y = apply(design.fact, 1, branin)

# create a pseudo large-dimension (2!) data set.
Y = cbind(y,2*y)

# create a list of the outputs
test.list = emlist(design.fact, Y)

# parallel lapply km to the list of outputs
test.km = mclapply(test.list,FUN=km.wrap, form = ~.)

# find a prediction in each element
xnew = c(0.5,0.5)

pred.list = mclapply(test.km,
                      FUN = predict,
                      newdata = matrix(xnew, nrow =1),
                      type = 'UK')

# predicted output
pred.list[[1]]$mean
pred.list[[2]]$mean


# ----------------------------------------------------------------------------
# Now, something more challenging.
# Emulate maps of FAMOUS, and test the accuracy/speed tradeoff
# for dimension reduction vs
# ----------------------------------------------------------------------------

library(ncdf4)
library(RColorBrewer)
library(fields)

rb <- brewer.pal(11, "RdBu")
bg <- brewer.pal(9, "BuGn")
yg <- brewer.pal(9, "YlGn")

source("https://raw.githubusercontent.com/dougmcneall/packages-git/master/emtools.R")
source("https://raw.githubusercontent.com/dougmcneall/packages-git/master/imptools.R")
source("https://raw.githubusercontent.com/dougmcneall/packages-git/master/vistools.R")

# Change these to something appropriate for the minute
# datdir <- "home/h01/hadda/famous/data/"
datdir <- "/Users/dougmcneall/Documents/work/famous/"
famousdir <- paste0(datdir,'famous_rename/')

load(paste0(datdir,'model_estimates.Rdata'))
load(paste0(datdir,'observed_estimates.Rdata'))
load(paste0(datdir,'famous_params_beta.RData'))

# Helper functions for working with FAMOUS data
f = function(s){
  strsplit(s, split = "a.pt")[[1]][1]
}

open.field = function(fn, var){
  # helper function to load a map of var from nc file
  nc = nc_open(fn)
  nc.var = ncvar_get(nc, var)
  nc.var	
}
load.spatial.ens = function(fn.list, var){
  # open all nc files in a list, vectorise, and concatenate to
  # an ensemble matrix, each row is a map
  field.list = lapply(fn.list, FUN = open.field, var = var)
  out = t(sapply(field.list,cbind)) # should do by columns
  out
}

remap.famous = function(dat,longs,lats, shift = FALSE){
  # reshape a map in vector form so that image() like functions
  # will plot it correctly
  mat = matrix(dat, nrow = length(longs), ncol = length(lats))[ ,length(lats):1]
  if(shift){
    block1.ix = which(longs < shift)
    block2.ix = which(longs > shift)
    mat.shift = rbind(mat[ block2.ix, ], mat[block1.ix, ]) 
    out = mat.shift
  }
  else{
    out = mat
  }
  out
}

# Functions for dimension reduced emulation
pc.project = function(pca,scores.em,Z.em,scale){
  # project principal components
  num.pc <- dim(scores.em)[2]
  if (scale){
    anom = ((pca$rotation[ ,1:num.pc] %*% t(scores.em))*pca$scale)
    anom.sd = ((pca$rotation[ ,1:num.pc] %*% t(Z.em))*pca$scale)          
  }
  else {
    anom = pca$rotation[ ,1:num.pc] %*% t(scores.em)
    anom.sd = pca$rotation[ ,1:num.pc] %*% t(Z.em)   
  }
  tens = t(anom + pca$center)
  return(list(tens = tens, anom.sd = anom.sd))
}

km.pc = function(Y, X, newdata, num.pc, scale = FALSE, center = TRUE, type = "UK", ...){
  # Base function for emulation of high dimensional data
  # with PCA and Gaussian Process emulator
  
  if (class(Y)!= 'prcomp'){
    pca = prcomp(Y,scale=scale, center=center)
  }
  else{
    pca = Y
  }
  if(is.matrix(newdata)!= TRUE){
    print('matrixifying newdata')
    newdata = matrix(newdata,nrow=1) 
  }
  scores.em = matrix(nrow=dim(newdata)[1],ncol=num.pc)
  Z.em = matrix(nrow=dim(newdata)[1],ncol=num.pc)
  
  for (i in 1:num.pc){
    # build the GP model
    fit = km(design=X, response=pca$x[,i])
    pred = predict(fit, newdata=newdata, type = type, ...)
    scores.em[ ,i] = pred$mean
    Z.em[ ,i] = pred$sd
  }
  proj = pc.project(pca, scores.em, Z.em, scale)
  return(list(tens=proj$tens, scores.em=scores.em, Z.em=Z.em, anom.sd=proj$anom.sd))
}


# --------------------------------------


# filename list
fn.list = as.list(paste(famousdir, dir(famousdir), sep = ""))
# model list 
modlist = sapply(dir(famousdir), f)
nmods = length(modlist)

# test open a file
nc = nc_open(fn.list[[1]])
npp.nc = ncvar_get(nc, "NPP_mm_srf")
lats = ncvar_get(nc,"latitude")
longs = ncvar_get(nc,"longitude")

# match the inputs to the correct order of the outputs
ix = match(modlist, as.character(params_beta$FULL_ID))
X = params_beta[ix,4:10]    # input parameters
d.X = ncol(X)           # input dimensions
X.norm = normalize(X)
# standard set of parameters
X.standard = c(0.875, 3, 0.03, 0.25, 36, 2, 0.5)
X.stan.norm = normalize(matrix(X.standard, nrow = 1), wrt = X)
colnames(X.stan.norm) = colnames(X.norm)

ndims = ncol(X)
nens = nrow(X)

# load an ensemble into a matrix
npp.ens = load.spatial.ens(fn.list, "NPP_mm_srf") * 1e08
npp.ens.trunc = npp.ens[ ,200:219] 

# find range of data for plotting
zl = range(npp.ens, na.rm = TRUE)
# plot first ensemble member
image.plot(longs, rev(lats), remap.famous(npp.ens[1,], longs, lats), col=yg )

# break the ensemble into a list
ens.list = emlist(X=X, Y=npp.ens)
ens.list.trunc = ens.list[230:250]

ptm = proc.time()
# parallel lapply km to the list of outputs
test.km = mclapply(ens.list.trunc,FUN=km.wrap, form = ~.)

# find a prediction in each element
#xnew = as.matrix(X[1, ], nrow=1)
xnew = X.stan.norm

pred.list = mclapply(test.km,
                     FUN = predict,
                     newdata = matrix(xnew, nrow =1),
                     type = 'UK')

direct.time = proc.time() - ptm
# Operating at approximately a second per emulator build on the Mac,
# which makes emulating the whole ensemble at around 30 minutes.

# pull out the mean prediction
sapply(pred.list, function(x) x$mean )

# collect up in a function
direct.pred = function(form,X,Y,Xnew,...){
  # Directly applies km in parallel to predict each column of an ensemble
  ens.list = emlist(X=X, Y=Y)
  km.list = mclapply(ens.list,FUN=km.wrap, form = form)
  pred.list = mclapply(km.list,
                       FUN = predict,
                       newdata = as.matrix(Xnew, nrow =1),
                       type = 'UK')
  out = sapply(pred.list, function(x) x$mean )
  out
}

ptm = proc.time()
direct.pred(form=~., X=X, Y=npp.ens[,200:219], Xnew=X.stan.norm )
direct.time = proc.time() - ptm

# for small jobs on the mac (e.g. 20 ensemble members), lapply is more efficient than mclapply

# --------------------------------------------------------------------
# Dimension reduction approach to building the emulator
# --------------------------------------------------------------------

# recode the function to take advantage

# Replace NAs with zeros for the moment to get prcomp to work
na.map = is.na(npp.ens)
npp.nona = npp.ens
npp.nona[na.map] = 0

npp.pc <- prcomp(npp.nona, scale = FALSE, center = TRUE)
image.plot(longs, rev(lats), remap.famous(npp.pc$rotation[,1], longs, lats))

test.pc = km.pc(Y=npp.nona, X=X.norm, newdata=X.stan.norm, num.pc=3)
image.plot(longs, rev(lats), remap.famous(test.pc$tens, longs, lats), col=yg )
world(add = TRUE) # I've solved the offset problem before

image.plot(longs, rev(lats), 
           remap.famous(test.pc$tens,longs, lats),
           col=yg)
map("world2", ylim=c(-90,90), xlim = c(0,360), add = TRUE)

# things to do
# Parallelise km.pc
# Put formulae into km.pc, and make sure that they work
# Run tests of point-by-point against dimension reduction approaches.
# How do errors change as the number of PCs goes up?
# How do things change as 

source("https://raw.githubusercontent.com/dougmcneall/packages-git/master/emtools.R")
source("https://raw.githubusercontent.com/dougmcneall/packages-git/master/imptools.R")
source("https://raw.githubusercontent.com/dougmcneall/packages-git/master/vistools.R")

# Note, get the raw version by clicking the 'download' version on github, and copying the path.
gitdaturl = 'https://github.com/dougmcneall/famous-git/raw/master/famous_forest_fraction.RData'
download.file(gitdaturl, 'myfile')
load('myfile')

yg <- brewer.pal(9, "YlGn")

na.map = is.na(bl.frac.ens)
bl.frac.nona = bl.frac.ens
bl.frac.nona[na.map] = 0

X = full_frac[,2:8]
X.norm = normalize(X)
X.stan.norm = normalize(matrix(X.standard, nrow = 1), wrt=X)

bl.frac.pc = km.pc(Y=bl.frac.nona, X=X.norm, newdata=X.stan.norm, num.pc=3)

image.plot(longs, rev(lats), 
           remap.famous(bl.frac.pc$tens,longs, lats),
           col=yg)
map("world2", ylim=c(-90,90), xlim = c(0,360), add = TRUE)

# What kind of error can we expect from cross validation using km.pc?
bl.cv.out = matrix(NA, nrow = nrow(bl.frac.nona), ncol = ncol(bl.frac.nona))

for(i in 1:nrow(X)){
  X.norm.trunc = X.norm[-i, ]
  Y.trunc = bl.frac.nona[-i, ]
  X.new = X.norm[i, ]
  bl.frac.pc = km.pc(Y=Y.trunc, X=X.norm.trunc, newdata=X.new, num.pc=6)
  bl.cv.out[i, ] = bl.frac.pc$tens 
}

image.plot(longs, rev(lats), 
           remap.famous(bl.cv.out[1, ],longs, lats),
           col=yg)
map("world2", ylim=c(-90,90), xlim = c(0,360), add = TRUE)

cv.err = bl.cv.out - bl.frac.ens

hist(c(cv.err))
mean(abs(c(cv.err)), na.rm=TRUE)

mae = function(x) {mean(abs(x), na.rm = TRUE)}

cv.err.mae.map = apply(cv.err, 2, mae)
image.plot(longs, rev(lats), 
           remap.famous(cv.err.mae.map,longs, lats),
           col=yg)
map("world2", ylim=c(-90,90), xlim = c(0,360), add = TRUE)

#
plot(c(bl.frac.nona), c(bl.cv.out))
abline(0,1, col = 'white')


library(parallel)
library(DiceKriging)

remap.famous = function(dat,longs,lats, shift = FALSE){
  # reshape a map in vector form so that image() like functions
  # will plot it correctly
  mat = matrix(dat, nrow = length(longs), ncol = length(lats))[ ,length(lats):1]
  if(shift){
    block1.ix = which(longs < shift)
    block2.ix = which(longs > shift)
    mat.shift = rbind(mat[ block2.ix, ], mat[block1.ix, ]) 
    out = mat.shift
  }
  else{
    out = mat
  }
  out
}

emlist = function(X, Y){
  # Create a list of objects to be emulated
  # X             design matrix with (nrow) instances of (ncol) inputs
  # Y             matrix of outputs, with one row
  #               for each row of X
  
  d = ncol(Y)
  em.list = vector(mode='list', length=d)
  
  for(i in 1:d){
    em.obj = NULL
    em.obj$X = X
    em.obj$y = Y[, i]
    em.list[[i]] = em.obj
  }
  em.list
}

# !N.B. need to check that we're passing the formula in correctly.
km.wrap = function(form, em, ...){
  # wrapper function to fit km model to list em.
  # each list element in em contains a design matrix X 
  # and an output vector y
  out = NA
  fit = try(km(form, design=em$X, response=em$y, control=list(trace=FALSE), ...), silent=TRUE)
  #if(class(fit) == "try-error"){
  #  out = NA
  #}
  #else{
  #  out = fit 
  #}
  out = fit
  out
}

# Functions for dimension reduced emulation
pc.project = function(pca,scores.em,Z.em,scale){
  # project principal components
  num.pc <- dim(scores.em)[2]
  if (scale){
    anom = ((pca$rotation[ ,1:num.pc] %*% t(scores.em))*pca$scale)
    anom.sd = ((pca$rotation[ ,1:num.pc] %*% t(Z.em))*pca$scale)          
  }
  else {
    anom = pca$rotation[ ,1:num.pc] %*% t(scores.em)
    anom.sd = pca$rotation[ ,1:num.pc] %*% t(Z.em)   
  }
  tens = t(anom + pca$center)
  return(list(tens = tens, anom.sd = anom.sd))
}

# need to sort this so that it gives out more of the dicekriging output

direct.pred = function(form,X,Y,Xnew,...){
  # Directly applies km in parallel to predict each column of an ensemble
  ens.list = emlist(X=X, Y=Y)
  km.list = mclapply(ens.list,FUN=km.wrap, form = form)
  pred.list = mclapply(km.list,
                       FUN = predict,
                       newdata = as.matrix(Xnew, nrow =1),
                       type = 'UK')
  out.mean = sapply(pred.list, function(x) x$mean )
  out.sd = sapply(pred.list, function(x) x$sd )
  
  return(list(mean=out.mean, sd=out.sd))
}


# OK, time to fix km.pc so that you can pass in a formula and use
# parallelisation

km.pc = function(Y, X, newdata, num.pc, scale = FALSE, center = TRUE, type = "UK", ...){
  # Base function for emulation of high dimensional data
  # with PCA and Gaussian Process emulator
  
  if (class(Y)!= 'prcomp'){
    pca = prcomp(Y,scale=scale, center=center)
  }
  else{
    pca = Y
  }
  if(is.matrix(newdata)!= TRUE){
    print('matrixifying newdata')
    newdata = matrix(newdata,nrow=1) 
  }
  scores.em = matrix(nrow=dim(newdata)[1],ncol=num.pc)
  Z.em = matrix(nrow=dim(newdata)[1],ncol=num.pc)
  
  for (i in 1:num.pc){
    # build the GP model
    fit = km(design=X, response=pca$x[,i])
    pred = predict(fit, newdata=newdata, type = type, ...)
    scores.em[ ,i] = pred$mean
    Z.em[ ,i] = pred$sd
  }
  proj = pc.project(pca, scores.em, Z.em, scale)
  return(list(tens=proj$tens, scores.em=scores.em, Z.em=Z.em, anom.sd=proj$anom.sd))
}


kmpar.pc = function(Y, X, newdata, num.pc, scale = FALSE, center = TRUE, type = "UK", ...){
  # Base function for emulation of high dimensional data
  # with PCA and Gaussian Process emulator
  
  if (class(Y)!= 'prcomp'){
    pca = prcomp(Y,scale=scale, center=center)
  }
  else{
    pca = Y
  }
  if(is.matrix(newdata)!= TRUE){
    print('matrixifying newdata')
    newdata = matrix(newdata,nrow=1) 
  }
  scores.em = matrix(nrow=dim(newdata)[1],ncol=num.pc)
  Z.em = matrix(nrow=dim(newdata)[1],ncol=num.pc)
  
  # replace loop with direct.pred
  pred = direct.pred(form=~., X=X, Y=pca$x[,1:num.pc], Xnew=newdata )
  
  scores.em[1,] = pred$mean
  Z.em[1,] = pred$sd
  # # loop
  # for (i in 1:num.pc){
  #   # build the GP model
  #   fit = km(design=X, response=pca$x[,i])
  #   pred = predict(fit, newdata=newdata, type = type, ...)
  #   scores.em[ ,i] = pred$mean
  #   Z.em[ ,i] = pred$sd
  # }
  
  proj = pc.project(pca, scores.em, Z.em, scale)
  return(list(tens=proj$tens, scores.em=scores.em, Z.em=Z.em, anom.sd=proj$anom.sd))
}

bl.frac.pc = km.pc(Y=bl.frac.nona, X=X.norm, newdata=X.stan.norm, num.pc=3)

bl.frac.pc.par = kmpar.pc(Y=bl.frac.nona, X=X.norm, newdata=X.stan.norm, num.pc=3)

image.plot(longs, rev(lats), 
           remap.famous(bl.frac.pc$tens,longs, lats),
           col=yg)
map("world2", ylim=c(-90,90), xlim = c(0,360), add = TRUE)

image.plot(longs, rev(lats), 
           remap.famous(bl.frac.pc.par$tens,longs, lats),
           col=yg)
map("world2", ylim=c(-90,90), xlim = c(0,360), add = TRUE)










