# famous_intro.R
# Some descriptive plots of the ensemble of FAMOUS
# Plots summary figures for the paper.
# (figure BL_obs_ensemble_mean_sd and figure frac_pairs)
# D. McNeall dougmcneall@gmail.com

# -------------------------------------------------------------
# 0. Packages, functions and data
# -------------------------------------------------------------
source('famous_common.R')


# -------------------------------------------------------------------
# 1. Pairs plot of input space and All forest fraction data
# -------------------------------------------------------------------
new.names <- c("F0\n(INPUT)","LAIMIN\n(INPUT)","NL0\n(INPUT)","RGROW\n(INPUT)","TUPP\n(INPUT)","Q10\n(INPUT)","VCRIT\n(INPUT)","AMAZON\n(OUTPUT)","SEASIA\n(OUTPUT)","CONGO\n(OUTPUT)","NAMERICA\n(OUTPUT)","GLOBAL\n(OUTPUT)")
colnames(full_frac) <- new.names

mypanel <- function(x,y,...){
  # pairs plot panel function
  ll <- par("usr") 
  rect(ll[1], ll[3], ll[2], ll[4], col="grey90" , border = NA)
  points(x, y, ...)
}


t.p <- function(x, y, labels, cex, font, ...){
  #ll <- par("usr") 
  #rect(ll[1], ll[3], ll[2], ll[4], col="grey90" , border = NA)
  
  text(x, y, labels, cex, font, col = 'black', ...)
  
}

X.standard.append <- c(X.standard, rep(NA, 5))
full_frac_stand <- rbind(full_frac, X.standard.append)

pdf(file = '../graphics/paper/frac_pairs.pdf', width = 9, height = 9)
#x11(width = 10, height = 10)
par(fg = 'grey90')
pairs(full_frac_stand, gap = 0.5,
      lower.panel = mypanel,
      upper.panel = NULL,
      label.pos = 0.7,
      text.panel = t.p,
      col = c(rep('black', 100), 'red'),
      cex = c(rep(0.6,100), 1),
      pch = c(rep(21, 100),19), 
      las = 2
      )
dev.off()

# -------------------------------------------------------------------
# 2. Visualise the Broadleaf forest fraction ensemble
# -------------------------------------------------------------------

open.frac.field <- function(fn,var,lev){
	
	# helper function to load a map of var from nc file
        # An edit of open.field, to account for different levels
  
	nc <- open.ncdf(fn)
	nc.var <- get.var.ncdf(nc, var)
        
	nc.var.lev <- nc.var[ , ,lev]
        nc.var.lev	
}

load.frac.ens <- function(fn.list, var, lev){
	
	# open all nc files in a list, vectorise, and concatenate to
	# an ensemble matrix, each row is a map
	
	field.list <- lapply(fn.list, FUN = open.frac.field, var = var, lev = lev)
	
	out <- t(sapply(field.list,cbind)) # should do by columns
	out
}

map.shift <- function(dat,longs,lats, shift = 180){

  block1.ix <- which(longs < shift)
  block2.ix <- which(longs > shift)
  
  dat.shift <- rbind(dat[ block2.ix, ], dat[block1.ix, ])

  dat.shift
  
}


# what is the range of the ensemble itself?
# lev = 1 is broadleaf forest (2 would be needleleaf)
bl.frac.ens <- load.frac.ens(fn.list = fn.list, var ='fracPFTs_mm_srf', lev = 1)

na.map <- is.na(bl.frac.ens)

bl.frac.ens.nona <- bl.frac.ens

na.map.onelayer <- is.na(bl.frac.ens.nona[1, ])

bl.frac.ens.nona[na.map] <- 0

# scale up to make everything a bit more stable
bl.frac.ens.scaled <- bl.frac.ens.nona * 100


# Plot the ensemble mean and standard deviation.
bl.frac.sd <- apply(bl.frac.ens, 2, sd)
bl.frac.mean <- apply(bl.frac.ens, 2, mean)
bl.zlim <- range(bl.frac.mean, na.rm = TRUE)
bl.frac.range <- apply(bl.frac.ens, 2, range)


# Load the observational vegetation fraction from Loveland et al. (2000)

veg_frac_obs_fn <- '../data/veg_frac_bl.nc'

nc <- open.ncdf(veg_frac_obs_fn)
nc.bl <- get.var.ncdf(nc, 'field1391')
nc.lats <- get.var.ncdf(nc, 'latitude')
nc.longs <- get.var.ncdf(nc, 'longitude')


pdf(file = '../graphics/paper/BL_obs_ensemble_mean_sd.pdf', width = 8, height = 6)
#dev.new(width = 8, height = 6)
par(mfrow = c(2,2), mar = c(2,3,3,3))

image.plot(nc.longs, nc.lats, map.shift(nc.bl, nc.longs, nc.lats, shift = 190),
           col = greens[2:9],
           axes = FALSE,
           main = '', xlab = '', ylab = '',
           legend.args = list(text = 'forest\nfraction', side = 3, line = 0.3)#,)
           )
text(3, -83, 'observations', pos = 4)

image.plot(longs, rev(lats), remap.famous(bl.frac.mean, longs, lats,  shift = 190),
           col = greens[2:9],
           axes = FALSE,
           main = '',
           xlab = '', ylab = '',
           legend.args = list(text = 'forest\nfraction', side = 3, line = 0.3),
           zlim = c(0,1)
           )
text(3, -83, 'ensemble mean', pos = 4)

image.plot(longs, rev(lats), remap.famous(bl.frac.sd, longs, lats,  shift = 190),
           col = greens[2:9],
           axes = FALSE,
           main = '',
           legend.args = list(text = 'forest\nfraction', side = 3, line = 0.3)#,
           #zlim = bl.zlim
           )
text(3, -83, 'ensemble standard deviation', pos = 4)

dev.off()
