# tropics_constraint.R
# Constraining FAMOUS using data on forest fraction
# Contents:
#
# 1. 2D projections of the implausibility of input space, given assumptions
# about the uncertainty of observations and model discrepancy.
# 
# 2. One-at-a-time sensitivity analysis using an emulator. Output
# is forest fraction.
# 2.a FAST sensitivity analysis
# 
# 3. How good is an emulator in
# leave-one-out mode?
#
# 4. How well can we constrain an input that
# we already know?
#
# 5. Two-at-a-time measure of implausibility
# (Here a function derived from the commented code above)
#
# 6. Optical depth plots (a la Vernon) - actually NROY density plots
#
# 7. Principal component emulation ofthe mapped forest fraction output
# (not terribly successful)


# 0. Packages, functions and data

source('famous_common.R')


# ------------------------------------------------------------------------
# How good is an the dicekriging emulator  for mean forest fractionin
# leave-one-out mode?
# Supplementary material.
# ------------------------------------------------------------------------

ynames <- c('Amazon', 'SE Asia', 'Africa', 'N America', 'Global')
#dev.new(width = 10, height = 7)
pdf(width = 10, height = 7, file = '../graphics/paper/frac_loo.pdf')

par(mfcol = c(2,3), cex.lab = 1.5, cex.axis = 1.3, oma = c(0.5,0.5, 0.5, 0.5))

for(i in 1:5){
  # uncomment to see the difference between

  y <- y.list[[i]]

  # use a linear response
  fit.lm <-  km(~., design = X.norm, response = y, covtype = "exp", control = list(pop.start = 50))
  # fit using just the mean
 # fit <-  km(design = X.norm, response = y)

  pred.lm <- leaveOneOut.km(fit.lm, type = 'UK')
#  pred <- leaveOneOut.km(fit,type = 'UK')
  
  rmse.lm <- sqrt(mean( (pred.lm$mean - y)^2 )) 
#  rmse <- sqrt(mean( (pred$mean - y)^2 ))
  
#  mae <- mean(abs(pred$mean - y))
#  mae.lm <- mean(abs(pred.lm$mean - y))
  
#  plot(y, pred$mean, main = rmse)
  #segments(y, (pred$mean - 2*pred$sd),y, (pred$mean + 2*pred$sd))
  #abline(0,1)

  #make sure the range of uncertainty is included
  ylim <- range(c( (pred.lm$mean - 2*pred.lm$sd),(pred.lm$mean + 2*pred.lm$sd)))
  
  plot(y, pred.lm$mean, main = ynames[i], type = 'n', las = 1,
       xlab = 'observed', ylab = 'predicted',
       ylim = c(0,1), cex.main = 1.3)
  segments(y, (pred.lm$mean - 2*pred.lm$sd),y, (pred.lm$mean + 2*pred.lm$sd), col = 'darkgrey')
  points(y, pred.lm$mean, pch = 19, col = 'black')
  legend('bottomright', legend = paste('rmse = ',round(rmse.lm,3)), cex = 1.5, bty = 'n')
  abline(0,1)
  
}

dev.off()





# ----------------------------------------------------------------------------------
# 1. 2D projections of the implausibility of input space, given assumptions
# about the uncertainty of observations and model discrepancy. No emulation yet.
# ----------------------------------------------------------------------------------
 

# Kernel density plot of input and output space
x11(width = 10, height = 10)
pairs(full_frac,panel = dfunc, gap = 0,upper.panel = NULL)

pairs(full_frac, upper.panel = dfunc, gap = 0, col = 'grey', pch = 20)


## Implausibility of each model run in the 2D space
# of NL0 and V_CRIT_ALPHA. Assuming an observational uncertainty
# of 0.1 (quite large I guess)
AMAZ.impl <- impl(em = AMAZ_MOD_FRAC, em.sd = 0, disc = 0, obs = obs_amazon, disc.sd = 0, obs.sd = 0.1)

pdf(width = 7, height = 7,file = '../graphics/impl_amaz_model.pdf')
cplot(x = X.norm[, 3], y = X.norm[ ,7], z = AMAZ.impl, cols = byr, pch = 19,
      cex = 1.5,
      xlab = 'NL0', ylab = 'V_CRIT_ALPHA', legend.title = 'implausibility'
      )
dev.off()






# NOTE This section needs attention.

pred <- predict(fit, newdata = X.oat, type = 'UK')
pred.impl <- impl(em = pred$mean, em.sd = pred$sd, disc = 0, obs = obs_amazon, disc.sd = 0, obs.sd = 0.1)

## Plot predicted AMAZON fraction output vs each input 
dev.new(width = 10, height = 6)
#pdf(width = 10, height = 6, file = '../graphics/amaz_oat_sens.pdf')
par(mfrow = c(2,4), las = 1)
for(i in 1: ncol(X.norm)){

  ix <- seq(from = ((i*n) - (n-1)), to =  (i*n), by = 1)
  
  plot(X[, i], AMAZ_MOD_FRAC, xlab = colnames(X)[i], col = 'grey')
  points(X.oat[ix, i], pred$mean[ix], pch = 21)
  abline(h = obs_amazon, col = 'red')  
}
#dev.off()


# Plot predicted AMAZON fraction output vs each input 
dev.new(width = 10, height = 6)
#pdf(width = 10, height = 6, file = '../graphics/amaz_oat_sens.pdf')
par(mfrow = c(2,4), las = 1)
for(i in 1: ncol(X.norm)){


  for j in 1:7
  
  ix <- seq(from = ((i*n) - (n-1)), to =  (i*n), by = 1)
  
  plot(X[, i], AMAZ_MOD_FRAC, xlab = colnames(X)[i], col = 'grey')
  points(X.oat[ix, i], pred$mean[ix], pch = 21)
  abline(h = obs_amazon, col = 'red')

}
#dev.off()


# Plot IMPLAUSIBILITY against each input varied OAAT

pdf(width = 10, height = 6, file = '../graphics/amaz_oat_imp.pdf')
par(mfrow = c(2,4), las = 1)
for(i in 1: ncol(X.norm)){

  ix <- seq(from = ((i*n) - (n-1)), to =  (i*n), by = 1)
  plot(X.oat[ix, i], pred.impl[ix], ylim = c(0,4), xlab = colnames(X)[i], ylab = 'implausibility') 
  abline(h = 3, col = 'red')
}
dev.off()


# ------------------------------------------------------------------------
# 4. How well can we constrain an input that
# we already know?
# -----------------------------------------------------------------------

#y <- y.list[[1]]
#ix.ex <- 2

y <- y.list[[1]]
ix.ex <- 2
X.train <- X.norm[-ix.ex, ]
X.target <- matrix(X.norm[ix.ex, ], nrow = 1)
colnames(X.target) <- colnames(X.train)


y.train <- y[-ix.ex]
y.target <- y[ix.ex]

fit <- km(~., design = X.train, response = y.train)

n <- 21
X.oat <- oaat.design(X.norm, n = n, hold = X.target)
pred <- predict(fit, newdata = X.oat, type = 'UK')

pred.impl <- impl(em = pred$mean, em.sd = pred$sd, disc = 0, obs = y.target, disc.sd = 0, obs.sd = 0)

# Plot IMPLAUSIBILITY against each input varied OAAT
dev.new(width = 10, height = 6)
par(mfrow = c(2,4))
for(i in 1: ncol(X.norm)){

  ix <- seq(from = ((i*n) - (n-1)), to =  (i*n), by = 1)
  plot(X.oat[ix, i], pred.impl[ix], ylim = c(0,4), xlab = colnames(X)[i]) 
  abline(h = 3, col = 'red')
  abline(v = X.target[i])
}

# how about a very large sample?

X.unif <- samp.unif(1000, rep(0,ndims), rep(1,ndims))
colnames(X.unif) <- colnames(X)
pred.unif <- predict(fit, newdata = X.unif, type = 'UK')
impl.unif <- impl(em = pred.unif$mean, em.sd = pred.unif$sd, disc = 0, obs = y.target, disc.sd = 0, obs.sd = 0)

ix.bt <- impl.unif < 3
#parcoord(X.unif[ix.bt, ])

sum(ix.bt)



# loop through all outputs and all members, find the
# proportion of inputs space ruled out

noutputs <- length(y.list)
nroy.prop.matrix <- matrix(nrow = nens, ncol = noutputs)
colnames(nroy.prop.matrix) <- c("AMAZ_MOD_FRAC", "SEASIA_MOD_FRAC", "CONGO_MOD_FRAC",
               "NAMERICA_MOD_FRAC", "GLOB_MOD_FRAC")

nsamp <- 5000
X.unif <- samp.unif(nsamp, rep(0,ndims), rep(1,ndims))
colnames(X.unif) <- colnames(X)

for(j in 1:noutputs){
 
  y <- y.list[[j]]

  for (i in 1:nens){      
    
    ix.ex <- i
    
    X.train <- X.norm[-ix.ex, ]
    X.target <- matrix(X.norm[ix.ex, ], nrow = 1)
    colnames(X.target) <- colnames(X.train)

    y.train <- y[-ix.ex]
    y.target <- y[ix.ex]
    
    fit <- km(~., design = X.train, response = y.train, control = list(trace = FALSE))

    pred.unif <- predict(fit, newdata = X.unif, type = 'UK')
    impl.unif <- impl(em = pred.unif$mean, em.sd = pred.unif$sd, disc = 0, obs = y.target, disc.sd = 0, obs.sd = 0)

    ix.bt <- impl.unif < 3
    nroy.prop <- sum(ix.bt) / nsamp
    nroy.prop.matrix[i, j] <- nroy.prop
    
  }

}

#dev.new(width = 6, height = 8)

pdf(width = 9, height = 8, file = '../graphics/potential_constraints_all.pdf')
par(mfrow = c(3,2), fg = 'white')

for(i in 1:5){

  hist((nroy.prop.matrix[, i] * 100), xlim = c(0,100),ylim = c(0,50),
       main = colnames(nroy.prop.matrix)[i],
       col = 'grey', axes = FALSE, xlab = 'NROY input space (%)')

axis(1, col = 'black')
axis(2, col = 'black')
  
}
dev.off()
      

# -------------------------------------------------------------------------
# 4. Two-at-a-time measure of implausibility
# -------------------------------------------------------------------------
## n <- 21 # number of grid divisions

## taat <- taat.design(X.norm, n = n, means = X.stan.norm)
## colnames(taat$des) <- colnames(X.norm)
## test <- data.frame(taat$des)

## fit <- km(~., design = X.norm, response = AMAZ_MOD_FRAC)
## taat.pred.stan <- predict(fit, newdata = X.stan.norm, type = 'UK')
## taat.pred <- predict(fit, newdata = test, type = 'UK')
## taat.impl <- impl(em = taat.pred$mean, em.sd = taat.pred$sd, disc = 0, obs = obs_amazon, disc.sd = 0, obs.sd = 0.1)

## dev.new(width = 7, height = 7)
## par(mar = c(0.5,0.5,0.5,0.5), oma = c(2,2,2,0.1), mgp = c(2,1,0), font.lab = 2, cex.lab = 1.5)
## nf <- layout(matrix(c(1,22,0,0,0,0,
## 2,7,0,0,0,0,
## 3,8,12,0,0,0,
## 4,9,13,16,0,0,
## 5,10,14,17,19,0,
## 6,11,15,18,20,21
## ), 6,6, byrow = TRUE))

## npc <- n*n

## for(i in 1:21){
  
## i.ix <- ((i * npc) - (npc - 1)) : (i* npc)
## x.ix <- taat$ix[1,i]
## y.ix <- taat$ix[2,i]

## cplotShort(taat$des[i.ix, x.ix],
##             taat$des[i.ix, y.ix],
##             z =  taat.impl[i.ix],
##             col = byr,
##             pch = 20,
##             xlab = colnames(X)[x.ix],
##             ylab = colnames(X)[y.ix],
##             axes = FALSE
##             )
## points(X.stan.norm[x.ix], X.stan.norm[y.ix], col = 'black', bg = 'green', cex = 2, pch = 21)

## if(i %in% c(1,2,3,4,5,6)) mtext(colnames(X)[y.ix], side = 2, line = 0.5, cex = 1)
## if(i %in% c(6,11,15,18,20,21)) mtext(colnames(X)[x.ix], side = 1, line = 1, cex = 1)

## }

## #zr <- range(taat.impl)
## zr <- c(0,3)

## par(mar = c(1,1,1,6))
## plot(1:10, type = 'n', axes = FALSE, xlab = '', ylab = '')

## legend('bottomleft',legend = 'Truth',pch = 21, pt.bg = 'green',cex = 1.5, pt.cex = 2, bty = 'n')

## image.plot(legend.only = TRUE,
##            zlim = zr,
##            col = byr,
##            legend.args = list(text = 'Implausibility', side = 3, line = 1),
##            legend.width = 2,
##            horizontal = FALSE
##            )


# -----------------------------------------------------------------
# 6. Optical depth plots (a la Vernon)
#
# -----------------------------------------------------------------
# What proportion of the emulated samples are below a threshold?

# Make a fucntion of it....
pairs.optical.depth <- function(X, y, X.target,thres, obs, obs.sd = 0, disc = 0, disc.sd = 0, n = 1000, title.text = '',
                           pdf.out = FALSE, filename = NULL){

  X.mins <- apply(X,2,min)
  X.maxes <- apply(X,2,max)
    
  X.unif <- samp.unif(n, mins = X.mins, maxes = X.maxes)

  colnames(X.unif) <- colnames(X)

  ## taat <- taat.design(X, n = n, means = X.target)
  ## colnames(taat$des) <- colnames(X)
  ## des <- data.frame(taat$des)

  fit <- km(~., design = X, response = y)

  pred <- predict(fit, newdata = X.unif, type = 'UK')
  taat.pred.stan <- predict(fit, newdata = X.target, type = 'UK')
 
  pred.impl <- impl(em = pred$mean, em.sd = pred$sd,
                    disc = disc, obs = obs, disc.sd = disc.sd, obs.sd = obs.sd)

  ix.bt <- pred.impl < thres

  #dev.new()
  pairs(X.unif[ix.bt==TRUE , ], panel = dfunc, gap = 0, upper.panel = NULL)

}


pdf(width = 7, height = 7, file = '../graphics/optical_global.pdf')
pairs.optical.depth(X = X.norm,
                    y = GLOB_MOD_FRAC,
                    X.target = X.stan.norm,
                    obs = obs_glob,
                    thres = 1,
                    obs.sd = 0.1,
                    disc = 0,
                    disc.sd = 0,
                    n = 10000,
                    title.text = 'GLOBAL'
                    )
dev.off()


pdf(width = 7, height = 7, file = '../graphics/optical_amazon.pdf')
pairs.optical.depth(X = X.norm,
                     y = AMAZ_MOD_FRAC,
                     X.target = X.stan.norm,
                    obs = obs_amazon,
                    thres = 1,
                    obs.sd = 0.1,
                    disc = 0,
                     disc.sd = 0,
                     n = 10000,
                     title.text = 'Amazon'
                            )
dev.off()

pdf(width = 7, height = 7, file = '../graphics/optical_congo.pdf')
pairs.optical.depth(X = X.norm,
                    y = CONGO_MOD_FRAC,
                    X.target = X.stan.norm,
                    obs = obs_congo,
                    thres = 1,
                    obs.sd = 0.1,
                    disc = 0,
                    disc.sd = 0,
                    n = 10000,
                    title.text = 'Congo'
                    )
dev.off()

pdf(width = 7, height = 7, file = '../graphics/optical_seasia.pdf')
pairs.optical.depth(X = X.norm,
                    y = SEASIA_MOD_FRAC,
                    X.target = X.stan.norm,
                    obs = obs_seasia,
                    thres = 1,
                    obs.sd = 0.1,
                    disc = 0,
                    disc.sd = 0,
                    n = 10000,
                    title.text = 'seasia'
                    )
dev.off()

pdf(width = 7, height = 7, file = '../graphics/optical_namerica.pdf')
pairs.optical.depth(X = X.norm,
                    y = NAMERICA_MOD_FRAC,
                    X.target = X.stan.norm,
                    obs = obs_namerica,
                    thres = 1,
                    obs.sd = 0.1,
                    disc = 0,
                    disc.sd = 0,
                    n = 10000,
                    title.text = 'namerica'
                    )
dev.off()




# -----------------------------------------------------------------------
# principal component emulation of the ensemble
# I think that the difference between the forests is getting washed
# out in the emulation. It might be better to do pixel-at-a-time
# emulation here.
# -----------------------------------------------------------------------
# -------------------------------------------------------------------
# 7. Emulate the mapped forest fraction output
# -------------------------------------------------------------------

## fn <- fn.list[[1]]
## nc <- open.ncdf(fn)
## names(nc$var$fracPFTs_mm_srf)


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

# lev = 1 is broadleaf forest (2 would be needleleaf)
bl.frac.ens <- load.frac.ens(fn.list = fn.list, var ='fracPFTs_mm_srf', lev = 1)


dev.new(width = 7, height = 5)
image.plot(longs, rev(lats), remap.famous(test[1, ], longs, lats), col = byr, axes = FALSE)
dev.new(width = 7, height = 5)
image.plot(longs, rev(lats), remap.famous(test[2, ], longs, lats), col = byr, axes = FALSE)


# have to play with the NAs for a moment
na.map <- is.na(bl.frac.ens)
bl.frac.ens.nona <- bl.frac.ens

bl.frac.ens.nona[na.map] <- 0
na.map.onelayer <- is.na(bl.frac.ens.nona[1, ])


# what is the range of the ensemble itself?

bl.frac.sd <- apply(bl.frac.ens.nona, 2, sd)
dev.new(width = 7, height = 5)
image.plot(longs, rev(lats), remap.famous(bl.frac.sd, longs, lats), col = byr, axes = FALSE)


bl.frac.pca <- prcomp(bl.frac.ens.nona, scale = FALSE, center = TRUE)

amazon.proj <- km.pc(X = X.norm, Y = bl.frac.ens.nona,num.pc = 1, newdata = plausible.amazon$X.out)
congo.proj <- km.pc(X = X.norm, Y = bl.frac.ens.nona, num.pc = 1, newdata = plausible.congo$X.out)


test.zero <-  km.pc(X = X.norm, Y = bl.frac.ens.nona, num.pc = 3, newdata = matrix(rep(0.0.5,7), nrow = 1))
test.one <-  km.pc(X = X.norm, Y = bl.frac.ens.nona, num.pc = 3, newdata = matrix(rep(0.95,7), nrow = 1))



dev.new(width = 7, height = 5)
image.plot(longs, rev(lats), remap.famous(test.zero$tens, longs, lats), col = byr,
           zlim = c(0,1), axes = FALSE)

dev.new(width = 7, height = 5)
image.plot(longs, rev(lats), remap.famous(test.one$tens, longs, lats), col = byr,
           zlim = c(0,1), axes = FALSE)



dev.new(width = 7, height = 5)
image.plot(longs, rev(lats), remap.famous(test.zero$tens - test.one$tens, longs, lats), col = byr, axes = FALSE)







amazon.mean <- apply(amazon.proj$tens, 2, mean)

congo.mean <- apply(congo.proj$tens, 2, mean)

dev.new(width = 7, height = 5)
image.plot(longs, rev(lats), remap.famous(amazon.mean, longs, lats), col = byr,
           zlim = c(0,1), axes = FALSE)

dev.new(width = 7, height = 5)
image.plot(longs, rev(lats), remap.famous(congo.mean, longs, lats), col = byr,
           zlim = c(0,1), axes = FALSE)



image.plot(longs, rev(lats), remap.famous(congo.mean - amazon.mean, longs, lats), col = byr, axes = FALSE)

X.best.amazon <- apply(plausible.amazon$X.out, 2, mean)
X.best.congo  <- apply(plausible.congo$X.out, 2, mean)


dev.new()
pairs(rbind(X.best.amazon , X.best.congo), xlim = c(0,1), ylim = c(0,1), upper.panel = NULL)
