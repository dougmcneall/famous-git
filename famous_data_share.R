# famous_data_share.R
# Collecting the underlying data for McNeall et al. (2016)
# Doug McNeall dougmcneall@gmail.com

source("https://raw.githubusercontent.com/dougmcneall/packages-git/master/emtools.R")
source("https://raw.githubusercontent.com/dougmcneall/packages-git/master/imptools.R")
source("https://raw.githubusercontent.com/dougmcneall/packages-git/master/vistools.R")

datdir = '../'
famousdir = '../famous_rename/'
load(paste0(datdir,'famous_params.RData'))
load(paste0(datdir,'famous_params_beta.RData'))
load(paste0(datdir,'model_estimates.Rdata'))
load(paste0(datdir,'observed_estimates.Rdata'))

library(RColorBrewer)
rb <- brewer.pal(11, "RdBu")
bg <- brewer.pal(9, "BuGn")
yg <- brewer.pal(9, "YlGn")

# Normalise the input space
X = params_beta[ ,4:10]
X.norm = normalize(X)
# standard set of parameters
X.standard = c(0.875, 3, 0.03, 0.25, 36, 2, 0.5)
X.stan.norm = normalize(matrix(X.standard, nrow = 1), wrt = X)

colnames(X.stan.norm) <- colnames(X.norm)

ndims = ncol(X)
nens = nrow(X)

# ---------------------------------------
# Bind everything up into a data frame
AMAZ_MOD_FRAC = unlist(model_amazon)
SEASIA_MOD_FRAC = unlist(model_seasia)
CONGO_MOD_FRAC = unlist(model_congo)
NAMERICA_MOD_FRAC = unlist(model_namerica)
GLOB_MOD_FRAC = unlist(model_glob)

y.list = list(AMAZ_MOD_FRAC, SEASIA_MOD_FRAC, CONGO_MOD_FRAC,
               NAMERICA_MOD_FRAC, GLOB_MOD_FRAC)

y.namelist = list('AMAZ_MOD_FRAC', 'SEASIA_MOD_FRAC', 'CONGO_MOD_FRAC',
                   'NAMERICA_MOD_FRAC', 'GLOB_MOD_FRAC')

full_frac = cbind(params_beta$FULL_ID,X, AMAZ_MOD_FRAC, SEASIA_MOD_FRAC,CONGO_MOD_FRAC, NAMERICA_MOD_FRAC, GLOB_MOD_FRAC)
colnames(full_frac)[1] = 'FULL_ID'
obs = matrix(c(obs_amazon, obs_seasia, obs_congo, obs_namerica, obs_glob), nrow = 1)
colnames(obs) <- c("AMAZON", "SEASIA", "CONGO", "NAMERICA", "GLOBAL")

# ----------------------------------------------------
library(ncdf4)

# Helper functions for working with FAMOUS data
f <- function(s){
  strsplit(s, split = "a.pt")[[1]][1]
}

open.frac.field = function(fn, var, lev){
  # helper function to load a map of var from nc file
  nc = nc_open(fn)
  nc.var = ncvar_get(nc, var)
  nc.var.lev = nc.var[ , ,lev]
  nc.var.lev
}
load.frac.ens = function(fn.list, var, lev){
  # open all nc files in a list, vectorise, and concatenate to
  # an ensemble matrix, each row is a map
  field.list = lapply(fn.list, FUN=open.frac.field, var=var, lev=lev)
  out = t(sapply(field.list,cbind)) # should do by columns
  out
}

remap.famous = function(dat,longs,lats, shift = FALSE){
  # reshape a map in vector form so that image() like functions
  # will plot it correctly
  mat = matrix(dat, nrow=length(longs), ncol=length(lats))[ ,length(lats):1]
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

match.list <- as.character(params$FULL_ID)
file.ix <- pmatch(match.list,dir(famousdir))
# filename list
fn.list <- as.list(paste(famousdir, dir(famousdir), sep = ""))

# test open a file
nc <- nc_open(fn.list[[1]])
npp.nc <- ncvar_get(nc, "NPP_mm_srf")
lats <- ncvar_get(nc,"latitude")
longs <- ncvar_get(nc,"longitude")

# lev = 1 is broadleaf forest (2 would be needleleaf)
bl.frac.ens = load.frac.ens(fn.list=fn.list[file.ix], var='fracPFTs_mm_srf', lev=1)
nl.frac.ens = load.frac.ens(fn.list=fn.list[file.ix], var='fracPFTs_mm_srf', lev=2)

# find range of data for plotting
zl <- c(0,1)

library(fields)
# plot first ensemble member
image.plot(longs, rev(lats), remap.famous(bl.frac.ens[1,], longs, lats), col=yg)

save(file='famous_forest_fraction.RData',
     full_frac,obs,lats,longs,bl.frac.ens, nl.frac.ens, X.standard)


  
  
  



