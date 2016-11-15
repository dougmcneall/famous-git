library(DiceKriging)
library(RColorBrewer)
library(MASS)
library(ncdf)
library(fields)
library(parallel)


source('/net/home/h01/hadda/code/R/packages-git/emtools.R')
source('/net/home/h01/hadda/code/R/packages-git/imptools.R')
source('/net/home/h01/hadda/code/R/packages-git/vistools.R')

datdir <- '../data/'
set.seed(42)

# ---------------------------------------
# Helper functions

f <- function(s){
  strsplit(s, split = "a.pt")[[1]][1]
}


open.field <- function(fn, var){
	
	# helper function to load a map of var from nc file
	
	nc <- open.ncdf(fn)
	nc.var <- get.var.ncdf(nc, var)
	nc.var	
}

load.spatial.ens <- function(fn.list, var){
	
	# open all nc files in a list, vectorise, and concatenate to
	# an ensemble matrix, each row is a map
	
	field.list <- lapply(fn.list, FUN = open.field, var = var)
	
	out <- t(sapply(field.list,cbind)) # should do by columns
	out
}

remap.famous <- function(dat,longs,lats, shift = FALSE){
	
  # reshape a map in vector form so that image() like functions
  # will plot it correctly
  
  mat <- matrix(dat, nrow = length(longs), ncol = length(lats))[ ,length(lats):1]
  
  if(shift){
    
    block1.ix <- which(longs < shift)
    block2.ix <- which(longs > shift)

    mat.shift <- rbind(mat[ block2.ix, ], mat[block1.ix, ]) 
    
    out <- mat.shift
  }
  
  else{
    out <- mat
  }
  
  out
}

pc.project <- function(pca,scores.em,Z.em,scale){

  # project principal components
  
  num.pc <- dim(scores.em)[2]

  if (scale){
    anom <- ((pca$rotation[ ,1:num.pc] %*% t(scores.em))*pca$scale)
    anom.sd <- ((pca$rotation[ ,1:num.pc] %*% t(Z.em))*pca$scale)          
  }
  
  else {
    anom <- pca$rotation[ ,1:num.pc] %*% t(scores.em)
    anom.sd <- pca$rotation[ ,1:num.pc] %*% t(Z.em)   
  }
  
  tens <- t(anom + pca$center)

  return(list(tens = tens, anom.sd = anom.sd))
}


km.pc <- function(Y, X, newdata, num.pc, scale = FALSE, center = TRUE, type = "UK", ...){

  # Base function for emulation of high dimensional data
  # with PCA and Gaussian Process emulator

  if (class(Y)!= 'prcomp'){
    pca <- prcomp(Y,scale = scale, center = center)
  }
  
  else{
    pca <- Y
  }
  
  if(is.matrix(newdata)!= TRUE){
    print('matrixifying newdata')
    newdata <- matrix(newdata,nrow = 1) 
  }
  
    scores.em <- matrix(nrow = dim(newdata)[1],ncol = num.pc)
    Z.em <- matrix(nrow = dim(newdata)[1],ncol = num.pc)
    	
    for (i in 1:num.pc){
   	
   	# build the GP model
   	
   	fit <- km(design = X, response = pca$x[,i])
   	pred <- predict(fit, newdata = newdata, type = type, ...)
   	
   	scores.em[ ,i] <- pred$mean
   	Z.em[ ,i] <- pred$sd
   				
    }

  proj = pc.project(pca, scores.em, Z.em, scale)

  return(list(tens = proj$tens,scores.em = scores.em,Z.em = Z.em,anom.sd = proj$anom.sd))
}


prop.thres <- function(x, thres, above = FALSE){
  
  # propotion of vector x below a threshold thres
  
  n <- length(x)

  if(above) bt <- length(x[x > thres])
  
  else bt <- length(x[x < thres])

  prop <- bt/n
  
  prop  
}


# --------------------------------------
# some bits for the maps

load(paste0(datdir,'model_estimates.Rdata'))
load(paste0(datdir,'observed_estimates.Rdata'))

famousdir <- paste0(datdir,'famous_rename/')
fn.list <- as.list(paste(famousdir, dir(famousdir), sep = ""))

# The model list 
modlist <- sapply(dir(famousdir), f)
nmods <- length(modlist)

nc <- open.ncdf(fn.list[[1]])
npp.nc <- get.var.ncdf(nc, "NPP_mm_srf")

lats <- get.var.ncdf(nc,"latitude")
longs <- get.var.ncdf(nc,"longitude")

# ---------------------------------------
# pallettes
rb <- brewer.pal(11, "RdBu")
ryg <- brewer.pal(11, "RdYlGn")
pbg <- brewer.pal(9, "PuBuGn")
bg <- brewer.pal(9, "BuGn")
yg <- brewer.pal(9, "YlGn")
byr <- rev(brewer.pal(11,'RdYlBu'))
br <- rev(rb)
blues <-  brewer.pal(9,'Blues')
rblues <-  rev(blues)

greens <-  brewer.pal(9,'Greens')
ygb <- brewer.pal(9, "YlGnBu")
brbg <- brewer.pal(11, "BrBG")
yob <- brewer.pal(9, "YlOrBr")
yor <- brewer.pal(9, "YlOrRd")

acc <- brewer.pal(8,'Paired')

col.amaz <- acc[1]
col.namerica <- acc[2]
col.seasia <- acc[3]
 col.congo <- acc[4]
col.global <- acc[5]


pch.global <- 3
pch.amaz <- 1
pch.congo <- 2
pch.seasia <- 5
pch.namerica <- 4

# ---------------------------------------

# ---------------------------------------
# input space

#params <- read.csv(paste0(datdir,'ensemble_params_famous_dougformat_noBeta3.csv'), skip = 23)
load(paste0(datdir,'famous_params.RData'))
load(paste0(datdir,'famous_params_beta.RData'))

# Normalise the input space
X <- params_beta[ ,4:10]
X.norm <- normalize(X)
# standard set of parameters
X.standard <- c(0.875, 3, 0.03, 0.25, 36, 2, 0.5)
X.stan.norm <- normalize(matrix(X.standard, nrow = 1), wrt = X)

colnames(X.stan.norm) <- colnames(X.norm)

ndims <- ncol(X)
nens <- nrow(X)

# ---------------------------------------
# Bind everything up into a data frame
AMAZ_MOD_FRAC <- unlist(model_amazon)
SEASIA_MOD_FRAC <- unlist(model_seasia)
CONGO_MOD_FRAC <- unlist(model_congo)
NAMERICA_MOD_FRAC <- unlist(model_namerica)
GLOB_MOD_FRAC <- unlist(model_glob)

y.list <- list(AMAZ_MOD_FRAC, SEASIA_MOD_FRAC, CONGO_MOD_FRAC,
               NAMERICA_MOD_FRAC, GLOB_MOD_FRAC)

y.namelist <- list('AMAZ_MOD_FRAC', 'SEASIA_MOD_FRAC', 'CONGO_MOD_FRAC',
               'NAMERICA_MOD_FRAC', 'GLOB_MOD_FRAC')

full_frac <- cbind(X, AMAZ_MOD_FRAC, SEASIA_MOD_FRAC,CONGO_MOD_FRAC, NAMERICA_MOD_FRAC, GLOB_MOD_FRAC)


# ---------------------------------------
# temperature and precip data

paths <- readLines('filepaths_famous.txt')

match.list <- as.character(params$FULL_ID)

modsFromFilelist <- sub("/net/home/h01/hadda/famous/data/temp_precip_famous/", "", paths)

file.ix <- pmatch(match.list, modsFromFilelist)

colh <- c('Global', 'Amazon', 'Congo', 'SEAsia')

temps <-cbind(match.list, read.table('temps_forests.txt', header = FALSE, skip = 1)[file.ix , ])
colnames(temps) <- c('RUN',colh)

precips <- cbind(match.list, read.table('precips_forests.txt', header = FALSE, skip = 1)[file.ix , ])
colnames(precips) <- c('RUN', colh)

precips_obs <- read.table('precips_obs.txt', header = FALSE, skip = 1)
colnames(precips_obs) <- c('Global', 'Amazon', 'Congo', 'SEAsia')

temps_obs <- read.table('temps_obs.txt', header = FALSE, skip = 1)
colnames(temps_obs) <- c('Global', 'Amazon', 'Congo', 'SEAsia')


# Implausibility helper

inputs.set <- function(X, y, thres, obs, obs.sd = 0, disc = 0, disc.sd = 0, n = 100000, abt = FALSE){ 

  # find a set of inputs that are consistent with a particular
  # set of implausibility (either below or above)
  
  X.mins <- apply(X,2,min)
  X.maxes <- apply(X,2,max)
  
  X.unif <- samp.unif(n, mins = X.mins, maxes = X.maxes)
  colnames(X.unif) <- colnames(X)
  
  fit <- km(~., design = X, response = y, control = list(trace = FALSE))
  pred <- predict(fit, newdata = X.unif, type = 'UK')
  pred.impl <- impl(em = pred$mean, em.sd = pred$sd,
                    disc = disc, obs = obs, disc.sd = disc.sd, obs.sd = obs.sd)
  
  if(abt){
   # choose those above the threshold 
    ix.bt <- pred.impl > thres
  }
  
  else{
    ix.bt <- pred.impl < thres
  }
  
  X.out <- X.unif[ix.bt, ]
  
  return(list(X.out = X.out, fit = fit, X.unif = X.unif, pred = pred,pred.impl = pred.impl))   
 
}


dfunc.up <- function(x,y,...){
  require(MASS)
  require(RColorBrewer)
  
  br <- brewer.pal(9, 'Blues')
                                        # function for plotting 2d kernel density estimates in pairs() plot.
  kde <- kde2d(x,y)
  image(kde, col = br, add = TRUE)

}
