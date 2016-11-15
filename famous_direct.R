# famous_direct.R

# Direct emulation of Forest fraction of the FAMOUS
# model
#
# Contents:
#
# 1. Find a set of inputs consistent with the aggregated
# forest fraction data (area means), for each forest., in
#
# 2. Predict the Amzon using the "best" set of parameters from
# the other models, and vice versa.
#
# 3. Are there any places where the implausibility is low
# for both Congo AND Amazon?
#
# 4. Create maps of the forest fraction at the "best" values of the
# Amazon, the congo, and the difference between them.
# Pointwise emulation.


# -----------------------------------------------------------------
# 0. Packages, functions and data
# -----------------------------------------------------------------

source('famous_common.R')

# -----------------------------------------------------------------
# 1. Find a set of inputs consistent with the aggregated
# forest fraction data (area means), for each forest.
# Using a very stringent tolerance to uncertainty, in
# order to keep only the very best inputs.
# -----------------------------------------------------------------

plausible.amazon <- inputs.set(X = X.norm, y = AMAZ_MOD_FRAC,thres = 3,
                   obs = obs_amazon,
                   obs.sd = 0,
                   disc = 0,
                   disc.sd = 0.01,
                   n = 100000,
                   abt = FALSE)

plausible.congo <- inputs.set(X = X.norm, y = CONGO_MOD_FRAC,thres = 3,
                   obs = obs_congo,
                   obs.sd = 0,
                   disc = 0,
                   disc.sd = 0.01,
                   n = 100000,
                   abt = FALSE)

plausible.namerica <- inputs.set(X = X.norm, y = NAMERICA_MOD_FRAC,thres = 3,
                   obs = obs_namerica,
                   obs.sd = 0,
                   disc = 0,
                   disc.sd = 0.01,
                   n = 100000,
                   abt = FALSE)

plausible.seasia <- inputs.set(X = X.norm, y = SEASIA_MOD_FRAC,thres = 3,
                   obs = obs_seasia,
                   obs.sd = 0,
                   disc = 0,
                   disc.sd = 0.01,
                   n = 100000,
                   abt = FALSE)

plausible.global <- inputs.set(X = X.norm, y = GLOB_MOD_FRAC,thres = 3,
                   obs = obs_glob,
                   obs.sd = 0,
                   disc = 0,
                   disc.sd = 0.01,
                   n = 100000,
                   abt = FALSE)


#dev.new()
pdf(width = 7, height = 7, file = '../graphics/paper/best_inputs_amazon.pdf')
pairs(plausible.amazon$X.out, panel = dfunc.up, gap = 0, upper.panel = NULL)
dev.off()

#dev.new()
pdf(width = 7, height = 7, file = '../graphics/paper/best_inputs_congo.pdf')
pairs(plausible.congo$X.out, panel = dfunc.up, gap = 0, upper.panel = NULL)
dev.off()

#dev.new()
pdf(width = 7, height = 7, file = '../graphics/best_inputs_namerica.pdf')
pairs(plausible.namerica$X.out, panel = dfunc.up, gap = 0, upper.panel = NULL)
dev.off()

#dev.new()
pdf(width = 7, height = 7, file = '../graphics/best_inputs_seasia.pdf')
pairs(plausible.seasia$X.out, panel = dfunc.up, gap = 0, upper.panel = NULL)
dev.off()

#dev.new()
pdf(width = 7, height = 7, file = '../graphics/best_inputs_global.pdf')
pairs(plausible.global$X.out, panel = dfunc.up, gap = 0, upper.panel = NULL)
dev.off()


# -----------------------------------------------------------------
# 2. Predict the Amzon using the "best" set of parameters from
# the other models, and vice versa.
# -----------------------------------------------------------------

fit.amazon <- km(~.,  design = X.norm, response = AMAZ_MOD_FRAC)
fit.congo <- km(~.,  design = X.norm, response = CONGO_MOD_FRAC)
fit.namerica <- km(~.,  design = X.norm, response = NAMERICA_MOD_FRAC)
fit.seasia <- km(~.,  design = X.norm, response = SEASIA_MOD_FRAC)
fit.global <- km(~.,  design = X.norm, response = GLOB_MOD_FRAC)

# The Amazon at the other parameter settings
pred.amaz.best.congo <- predict(fit.amazon, newdata = plausible.congo$X.out, type = 'UK')
pred.amaz.best.namerica <- predict(fit.amazon, newdata = plausible.namerica$X.out, type = 'UK')
pred.amaz.best.seasia <- predict(fit.amazon, newdata = plausible.seasia$X.out, type = 'UK')
pred.amaz.best.global <- predict(fit.amazon, newdata = plausible.global$X.out, type = 'UK')

# The other forests at the Amazon settings 
pred.congo.best.amaz <- predict(fit.congo, newdata = plausible.amazon$X.out, type = 'UK')
pred.namerica.best.amaz <- predict(fit.namerica, newdata = plausible.amazon$X.out, type = 'UK')
pred.seasia.best.amaz <- predict(fit.seasia, newdata = plausible.amazon$X.out, type = 'UK')
pred.global.best.amaz <- predict(fit.global, newdata = plausible.amazon$X.out, type = 'UK')


pdf(file = '../graphics/paper/best_inputs_swaps_hists_Paired.pdf', width = 7, height = 7)

par(mfrow = c(2,1), fg = 'white', las = 1)
br <- seq(from = -0.1, to = 1.2, by = 0.02)

hist.amaz.congo <- hist(pred.amaz.best.congo$mean, plot = FALSE, breaks = br)
hist.amaz.namerica <- hist(pred.amaz.best.namerica$mean, plot = FALSE, breaks = br)
hist.amaz.seasia <- hist(pred.amaz.best.seasia$mean, plot = FALSE, breaks = br)
hist.amaz.global <- hist(pred.amaz.best.global$mean, plot = FALSE, breaks = br)

plot(hist.amaz.global, xlim = c(0,1), main = 'Amazon at various best forest parameters',
     xlab = 'Broadleaf Forest Fraction', ylab = 'emulated members', col = col.global, ylim = c(0,6000), axes = FALSE)

plot(hist.amaz.namerica, xlim = c(0,1), add = TRUE, col = col.namerica)
plot(hist.amaz.seasia, xlim = c(0,1), add = TRUE, col = col.seasia)
plot(hist.amaz.congo, xlim = c(0,1), add = TRUE, col = col.congo)

legend('topright', legend = c('Global', 'N America', 'SE Asia', 'Africa'), fill = c(col.global, col.namerica, col.seasia, col.congo), border = 'white', text.col = c(col.global, col.namerica, col.seasia, col.congo))

axis(1, col = 'grey', line = 1)
axis(2, col = 'grey')
par(xpd = NA)
rug(obs_amazon, col = col.amaz, lwd = 4, line = 1.2)
text(obs_amazon,-350,  'Amazon', cex = 1, col = col.amaz)

hist.congo.amaz <- hist(pred.congo.best.amaz$mean, plot = FALSE, breaks = br)
hist.namerica.amaz <- hist(pred.namerica.best.amaz$mean, plot = FALSE, breaks = br)
hist.seasia.amaz <- hist(pred.seasia.best.amaz$mean, plot = FALSE, breaks = br)
hist.global.amaz <- hist(pred.global.best.amaz$mean, plot = FALSE, breaks = br)

plot(hist.global.amaz, xlim = c(0,1), main = 'Other forests at best Amazon parameters',
     xlab = 'Broadleaf Forest Fraction', ylab = 'emulated members', col = col.global, axes = FALSE, ylim = c(0,8000))

plot(hist.congo.amaz, xlim = c(0,1), add = TRUE, col = col.congo)
plot(hist.namerica.amaz, xlim = c(0,1), add = TRUE, col = col.namerica)
plot(hist.seasia.amaz, xlim = c(0,1), add = TRUE, col = col.seasia)

axis(1, col = 'grey', line = 1)
axis(2, col = 'grey')
rug(obs_congo, col = col.congo, lwd = 4, line = 1.2)
text(obs_congo,-300,  'Africa', cex = 1, col = col.congo)

rug(obs_namerica, col = col.namerica, lwd = 4, line = 1.2)
text(obs_namerica,-300,  'N America', cex = 1, col = col.namerica)

rug(obs_seasia, col = col.seasia, lwd = 4, line = 1.2)
text(obs_seasia,-900,  'SE Asia', cex = 1, col = col.seasia)

rug(obs_glob, col = col.global, lwd = 4, line = 1.2)
text(obs_glob,-900,  'Global', cex = 1, col = col.global)

dev.off()


# Implausibility of the standard/default parameter settings


# -----------------------------------------------------------------
# 3. Are there any places where the implausibility is low
# for both Africa AND Amazon?
# -----------------------------------------------------------------

# another function, that builds implausibility, given a number of outputs
multiple.inputs.set <- function(X, Y, thres, obs, obs.sd, disc, disc.sd, n = 100000, abt = FALSE){ 
                                        # find a set of inputs that are consistent with a particular
                                        # set of implausibility (either below or above)
  
  X.mins <- apply(X,2,min)
  X.maxes <- apply(X,2,max)
  
  X.unif <- samp.unif(n, mins = X.mins, maxes = X.maxes)
  colnames(X.unif) <- colnames(X)
                                        # how many output dimensions
  d <- ncol(Y)
                                        # implausibility matrix holder
  imp.mat <- matrix(ncol = d, nrow = n)
  
  for(i in 1:d){
    
    y <- Y[ ,i]
    obs
                                        # loop through the outputs and build an implausibility matrix
    fit <- km(~., design = X, response = y, control = list(trace = FALSE))
    pred <- predict(fit, newdata = X.unif, type = 'UK')
    pred.impl <- impl(em = pred$mean, em.sd = pred$sd,
                      disc = disc[, i], obs = obs[i], disc.sd = disc.sd[, i], obs.sd = obs.sd[ ,i ])
    
    imp.mat[, i] <- pred.impl 
  }
  
  if(abt){
                                        # choose those above the treshold 
    ix.bt <- apply(imp.mat > thres, 1, all)
  }
  
  else{
    ix.bt <- apply(imp.mat < thres, 1, all)
  }  
  
  X.out <- X.unif[ix.bt, ]
  
  return(list(X.out = X.out,X.unif = X.unif, imp.mat = imp.mat))   
}


# just look at the amazon and the Africa to start with
Y <-  cbind(AMAZ_MOD_FRAC,CONGO_MOD_FRAC)
Y.all <- cbind(AMAZ_MOD_FRAC, SEASIA_MOD_FRAC,CONGO_MOD_FRAC, NAMERICA_MOD_FRAC)

## plausible.both <- multiple.inputs.set(X = X.norm,
##                                       Y = Y,
##                                       thres = 3,
##                                       obs = c(obs_amazon, obs_congo),
##                                       obs.sd = cbind(rep(0,100), rep(0,100)), 
##                                       disc = cbind(rep(0,100), rep(0,100)),
##                                       disc.sd = cbind(rep(0.01,100), rep(0.01,100)),
##                                       n = 10000,
##                                       abt = FALSE)


# ---------------------------------------------------------------------------------------
# How much space do you cut out with a "credible" estimate for observational uncertainty?
# ---------------------------------------------------------------------------------------
plausible.credible <- multiple.inputs.set(X = X.norm,
                                     Y = Y.all,
                                     thres = 3,
                                     obs = c(obs_amazon,obs_seasia, obs_congo,obs_namerica),
                                     obs.sd = cbind(rep(0.05,100), rep(0.05,100), rep(0.05,100), rep(0.05,100) ), 
                                     disc = cbind(rep(0,100), rep(0,100), rep(0.0,100), rep(0,100) ),
                                     disc.sd = cbind(rep(0,100), rep(0,100),rep(0,100),rep(0,100)),
                                     n = 100000,
                                     abt = FALSE)

pdf(width = 7, height = 7, file = '../graphics/paper/credible_NROY.pdf')
pairs(plausible.credible$X.out, panel = dfunc.up, gap = 0, upper.panel = NULL)
dev.off()


(dim(plausible.credible$X.out)[1]) / 100000

# Predict forest fraction in the credible NROY space
pred.amaz.overlap <- predict(fit.amazon, newdata = plausible.credible$X.out, type = 'UK')
pred.congo.overlap <- predict(fit.congo, newdata = plausible.credible$X.out, type = 'UK')
pred.namerica.overlap <- predict(fit.namerica, newdata = plausible.credible$X.out, type = 'UK')
pred.seasia.overlap <- predict(fit.seasia, newdata = plausible.credible$X.out, type = 'UK')
pred.global.overlap <- predict(fit.global, newdata = plausible.credible$X.out, type = 'UK')

hbreaks <- seq(from = 0, to = 1, by = 0.02)

pdf(file = '../graphics/paper/credible_NROY_hists.pdf', width = 6, height = 8)
#dev.new(width=6, height=8)
par(mfrow= c(5,1),fg = 'white', las = 1, mar = c(3,1,2,0), oma = c(3,5,0,1), cex.axis = 1.3)

#dev.new()
hist(pred.amaz.overlap$mean, xlim=c(0,1), breaks=hbreaks, prob=TRUE,
     col=col.amaz, axes=FALSE,
     main='', xlab='', ylab='')
mtext(text='Amazon', side=3, line=-0.7, col='black', adj = 0.03, cex = 1.2)
axis(1, col='grey', line=1)
axis(2, col = 'grey')
par(xpd = NA)
rug(obs_amazon, col = col.amaz, lwd=5, line = 1.2)

hist(pred.congo.overlap$mean, xlim=c(0,1), breaks=hbreaks, prob=TRUE,
     col=col.congo, axes=FALSE,
     main='', xlab='', ylab='')
mtext(text='Congo', side=3, line=-0.7, col='black', adj = 0.03, cex = 1.2)
axis(1, col='grey', line=1)
axis(2, col='grey')
par(xpd=NA)
rug(obs_congo, col = col.congo, lwd=5, line = 1.2)

hist(pred.namerica.overlap$mean, xlim=c(0,1), breaks=hbreaks, prob=TRUE,
     col=col.namerica, axes=FALSE,
     main='', xlab='', ylab='')
mtext(text='N America', side=3, line=-0.7, col='black', adj = 0.03, cex = 1.2)
axis(1, col='grey', line=1)
axis(2, col='grey')
par(xpd=NA)
rug(obs_namerica, col=col.namerica, lwd=5, line=1.2)

hist(pred.seasia.overlap$mean, xlim = c(0,1), breaks = hbreaks, prob=TRUE,
     col = col.seasia, axes = FALSE,
     main = '', xlab = '', ylab = '')
mtext(text='SE Asia', side=3, line=-0.7, col='black', adj = 0.03, cex = 1.2)
axis(1, col = 'grey', line = 1)
axis(2, col = 'grey')
par(xpd = NA)
rug(obs_seasia, col = col.seasia, lwd = 5, line = 1.2)

hist(pred.global.overlap$mean, xlim = c(0,1), breaks = hbreaks, prob=TRUE,
     col = col.global, axes = FALSE,
     main = '', xlab = '', ylab = '')
mtext(text='Global', side=3, line=-0.7, col='black', adj = 0.03, cex = 1.2)
axis(1, col = 'grey', line = 1)
axis(2, col = 'grey')
par(xpd = NA)
rug(obs_glob, col = col.global, lwd = 5, line = 1.2)

mtext(outer=TRUE, side=2, text='Density', line=2.5, col='black',  las=0, cex=1.3) 
mtext(outer=TRUE, side=1, text='Forest Fraction', line=2, col='black', fg = 'black', las=0, cex=1.3)

dev.off()


## # contrast the amazon with all but the amazon
## Y.allbutamazon <- cbind(SEASIA_MOD_FRAC,CONGO_MOD_FRAC, NAMERICA_MOD_FRAC)

## plausible.allbutamazon <- multiple.inputs.set(X = X.norm,
##                                      Y = Y.allbutamazon,
##                                      thres = 3,
##                                      obs = c(obs_seasia, obs_congo,obs_namerica),
##                                      obs.sd = cbind(rep(0.01,100), rep(0.01,100), rep(0.01,100) ), 
##                                      disc = cbind(rep(0,100), rep(0.0,100), rep(0,100) ),
##                                      disc.sd = cbind(rep(0,100),rep(0,100),rep(0,100)),
##                                      n = 100000,
##                                      abt = FALSE)

## pdf(width = 7, height = 7, file = '../graphics/plausible_all_but_amazon.pdf')
## pairs(plausible.allbutamazon$X.out, panel = dfunc.up, gap = 0, upper.panel = NULL)
## dev.off()


## (dim(plausible.allbutamazon$X.out)[1]) / 100000







# Recast as a "tolerence to error" idea: How does the NROY space react to a varying
# uncertainty on the observations? From "absolute truth" to "just guessing".

# A vector of observational uncertainty
#disc.uncertainty.vec <- seq(from = 0.005, to = 0.25, by = 0.005)
disc.uncertainty.vec <- seq(from = 0.005, to = 0.25, by = 0.01)
prop.both <- rep(NA, length(disc.uncertainty.vec))

n.samples <- 10000


# this looks at how much space is ruled out if you use all
# the data
for (i in 1:length(disc.uncertainty.vec)){

  print(i)
  du <- disc.uncertainty.vec[i]
  plausible.both <- multiple.inputs.set(X = X.norm,
                                        Y = Y,
                                        thres = 3,
                                        obs = c(obs_amazon, obs_congo),
                                        obs.sd = cbind(rep(0,100), rep(0,100)), 
                                        disc = cbind(rep(0,100), rep(0,100)),
                                        disc.sd = cbind(rep(du,100), rep(du,100)),
                                        n = n.samples,
                                        abt = FALSE)

  prop.both[i] <- nrow(plausible.both$X.out) / n.samples
  
}


pdf(height = 7, width = 7,file = '../graphics/Prop_NROY_amaz_congo_unc.pdf')
par(yaxs = 'i', xaxs = 'i')
plot(disc.uncertainty.vec, prop.both, pch = 19, type = 'o', xlab = 'observational uncertainty', ylab = 'NROY proportion', main = 'Using Amazon & Africa observations')

dev.off()


prop.all <- rep(NA, length(disc.uncertainty.vec))
# Now, have a look at what the proportion ruled out is with various combinations of the data
for (i in 1:length(disc.uncertainty.vec)){

  print(i)
  du <- disc.uncertainty.vec[i]
  plausible.all <- multiple.inputs.set(X = X.norm,
                                        Y = Y.all,
                                        thres = 3,
                                        obs = c(obs_amazon,obs_seasia, obs_congo, obs_namerica),
                                        obs.sd = cbind(rep(0,100), rep(0,100), rep(0,100), rep(0,100)), 
                                        disc = cbind(rep(0,100), rep(0,100), rep(0,100), rep(0,100)),
                                        disc.sd = cbind(rep(du,100), rep(du,100), rep(du,100), rep(du,100)),
                                        n = n.samples,
                                        abt = FALSE)

  prop.all[i] <- nrow(plausible.all$X.out) / n.samples
  
}



prop.non.amaz <- rep(NA, length(disc.uncertainty.vec))
# Now, have a look at what the proportion ruled out is with various combinations of the data
for (i in 1:length(disc.uncertainty.vec)){

  print(i)
  du <- disc.uncertainty.vec[i]
  plausible.non.amaz <- multiple.inputs.set(X = X.norm,
                                        Y = Y.all[, -1],
                                        thres = 3,
                                        obs = c(obs_seasia, obs_congo, obs_namerica),
                                        obs.sd = cbind(rep(0,100), rep(0,100), rep(0,100)), 
                                        disc = cbind(rep(0,100), rep(0,100), rep(0,100)),
                                        disc.sd = cbind(rep(du,100), rep(du,100), rep(du,100)),
                                        n = n.samples,
                                        abt = FALSE)

  prop.non.amaz[i] <- nrow(plausible.non.amaz$X.out) / n.samples
  
}


prop.nroy <- function(X, y, thres, obs, obs.sd, disc, disc.sd.vec, n, abt=FALSE){

  prop <- rep(NA, length(disc.sd.vec))
  
  for(i in 1:length(disc.sd.vec)){
    
    plausible <- inputs.set(X = X, y = y,thres = thres,
                            obs = obs,
                            obs.sd = 0,
                            disc = 0,
                            disc.sd = disc.sd.vec[i],
                            n = n,
                            abt = abt)
    
    prop[i] <- nrow(plausible$X.out) / n.samples
    
  }
  
  prop
  
}


# Calculate how much space is ruled out as you up the tolerance to
# uncertainty for individual data streams

prop.amaz <- prop.nroy(X = X.norm, y = AMAZ_MOD_FRAC,thres = 3,
                       obs = obs_amazon,
                       obs.sd = 0,
                       disc = 0,
                       disc.sd = disc.uncertainty.vec,
                       n = n.samples,
                       abt = FALSE)

prop.congo <- prop.nroy(X = X.norm, y = CONGO_MOD_FRAC,thres = 3,
                        obs = obs_congo,
                        obs.sd = 0,
                        disc = 0,
                        disc.sd = disc.uncertainty.vec,
                        n = n.samples,
                        abt = FALSE)

prop.seasia <- prop.nroy(X = X.norm, y = SEASIA_MOD_FRAC,thres = 3,
                         obs = obs_seasia,
                         obs.sd = 0,
                         disc = 0,
                         disc.sd = disc.uncertainty.vec,
                         n = n.samples,
                         abt = FALSE)

prop.namerica <- prop.nroy(X = X.norm, y = NAMERICA_MOD_FRAC,thres = 3,
                           obs = obs_namerica,
                           obs.sd = 0,
                           disc = 0,
                           disc.sd = disc.uncertainty.vec,
                           n = n.samples,
                           abt = FALSE)



 

pdf(height = 7, width = 7,file = '../graphics/paper/Prop_NROY_tolerance_unc.pdf')
#dev.new()
par(las = 1)
plot(disc.uncertainty.vec, prop.all, pch = 19, type = 'n', xlab = 'Tolerence to error (standard deviations)', ylab = 'NROY proportion')
grid()

points(disc.uncertainty.vec, prop.all, col = 'black', type = 'o', pch = 15)

points(disc.uncertainty.vec, prop.non.amaz, col = col.global, type = 'o', pch = 22)

points(disc.uncertainty.vec, prop.amaz, col = col.amaz, type = 'o', pch = 19)
points(disc.uncertainty.vec, prop.seasia, col = col.seasia, type = 'o', pch = 17)
points(disc.uncertainty.vec, prop.namerica, col = col.namerica, type = 'o', pch = 24)
points(disc.uncertainty.vec, prop.congo, col = col.congo, type = 'o', pch = 21)


legend('right', legend = c('Amazon', 'Africa','SE Asia', 'N America','All', 'All except Amazon'), col = c(col.amaz, col.congo, col.seasia, col.namerica, 'black',col.global),lwd = 1, pch = c(19,21,17,24,15,22))  

dev.off()


# To what extent are the input spaces shared? What proportion of input space
# gives the same value for 'rejected/accepted'?

input.overlap <- function(X.unif, imp.mat, thres = 3){
  # Measures the degree of shared 'not implausible'
  # input space

  bt <- imp.mat < thres

  bt.sum <-  apply(bt, 1, sum)

  ix <- bt.sum == ncol(imp.mat) | bt.sum == 0 # all above or all below threshold

  X.shared <- X.unif[ix, ]

  prop.shared <- nrow(X.shared) / nrow(X.unif)
  
  return(list(prop.shared=prop.shared, X.shared=X.shared))
  
}


perm.ix <- expand.grid(1:4, 1:4)
shared.vec <- rep(NA, nrow(perm.ix))

for(i in 1:nrow(perm.ix)){

  shared <- input.overlap(plausible.credible$X.unif,
                          cbind(plausible.credible$imp.mat[, perm.ix[i, 1]],
                          plausible.credible$imp.mat[, perm.ix[i, 2]])
                          )
  shared.vec[i] <- shared$prop.shared
  
}

shared.mat <- cbind(perm.ix,shared.vec, colnames(Y.all)[perm.ix[,1]],colnames(Y.all)[perm.ix[,2]] )
colnames(shared.mat) <- c('ix1', 'ix2','prop.shared', 'forest1', 'forest2')










# ------------------------------------------------------------------------------
# Choose a specific value for illustration. Use all data, and all except Amazon,
# with obs and discrepancy 1sd = 0.00333.
# This should correspond to 95% CI of pm 0.1 for both obs and discrepancy
# ------------------------------------------------------------------------------







# ------------------------------------------------------------------------------
# Vary observational and model discrepancy uncertainty simultaneously,
# leading to a 2D plot of observational and model discrepancy uncertainty
# ------------------------------------------------------------------------------

obs.sd.vec <- seq(from = 0.01, to = 0.1, by = 0.01)
disc.sd.vec <- seq(from = 0.01, to = 0.1, by = 0.01)
prop.nroy.obs_sd<- function(X, y, thres, obs, obs.sd.vec, disc, disc.sd.vec, n, abt=FALSE){

 # prop <- rep(NA, length(disc.sd.vec))
  prop <- matrix(NA, nrow = length(disc.sd.vec), ncol = length(obs.sd.vec))
  
  for(i in 1:length(disc.sd.vec)){
    for (j in 1:length(obs.sd.vec)){
      
      plausible <- inputs.set(X = X, y = y,thres = thres,
                              obs = obs,
                              obs.sd = obs.sd.vec[j],
                              disc = 0,
                              disc.sd = disc.sd.vec[i],
                              n = n,
                              abt = abt)
      prop[i, j] <- nrow(plausible$X.out) / n.samples
    }
  }
  prop
}

prop.amaz.obs_sd <- prop.nroy.obs_sd(X = X.norm, y = AMAZ_MOD_FRAC,thres = 3,
                                 obs = obs_amazon,
                                 obs.sd.vec = obs.sd.vec,
                                 disc = 0,
                                 disc.sd = disc.sd.vec,
                                 n = n.samples,
                                 abt = FALSE)

prop.congo.obs_sd <- prop.nroy.obs_sd(X = X.norm, y = CONGO_MOD_FRAC,thres = 3,
                                 obs = obs_congo,
                                 obs.sd.vec = obs.sd.vec,
                                 disc = 0,
                                 disc.sd = disc.sd.vec,
                                 n = n.samples,
                                 abt = FALSE)

prop.seasia.obs_sd <- prop.nroy.obs_sd(X = X.norm, y = SEASIA_MOD_FRAC,thres = 3,
                                 obs = obs_seasia,
                                 obs.sd.vec = obs.sd.vec,
                                 disc = 0,
                                 disc.sd = disc.sd.vec,
                                 n = n.samples,
                                 abt = FALSE)

prop.namerica.obs_sd <- prop.nroy.obs_sd(X = X.norm, y = NAMERICA_MOD_FRAC,thres = 3,
                                 obs = obs_namerica,
                                 obs.sd.vec = obs.sd.vec,
                                 disc = 0,
                                 disc.sd = disc.sd.vec,
                                 n = n.samples,
                                 abt = FALSE)

dev.new()
image.plot(disc.sd.vec, obs.sd.vec, prop.amaz.obs_sd,
           xlab = 'Discrepancy Standard Deviation', ylab = 'Observation Standard deviation',
           main = 'Amazon', col = byr)

dev.new()
image.plot(disc.sd.vec, obs.sd.vec, prop.congo.obs_sd,
           xlab = 'Discrepancy Standard Deviation', ylab = 'Observation Standard deviation',
           main = 'Congo', col = byr)

dev.new()
image.plot(disc.sd.vec, obs.sd.vec, prop.seasia.obs_sd,
           xlab = 'Discrepancy Standard Deviation', ylab = 'Observation Standard deviation',
           main = 'SE Asia', col = byr)

dev.new()
image.plot(disc.sd.vec, obs.sd.vec, prop.namerica.obs_sd,
           xlab = 'Discrepancy Standard Deviation', ylab = 'Observation Standard deviation',
           main = 'N America', col = byr)



# If the default parameters are correct, what does our discrepancy look like?
standard.amazon <- predict(fit.amazon, newdata = X.stan.norm, type = 'UK')
standard.congo <- predict(fit.congo, newdata = X.stan.norm, type = 'UK')
standard.seasia <- predict(fit.seasia, newdata = X.stan.norm, type = 'UK')
standard.namerica <- predict(fit.namerica, newdata = X.stan.norm, type = 'UK')
standard.global <- predict(fit.global, newdata = X.stan.norm, type = 'UK')

disc.standard.amazon <- standard.amazon$mean - obs_amazon
disc.standard.congo <- standard.congo$mean - obs_congo
disc.standard.seasia <- standard.seasia$mean - obs_seasia
disc.standard.namerica <- standard.namerica$mean - obs_namerica
disc.standard.global <- standard.global$mean - obs_glob
#

# What is the implausibility and maximum implausibility at the default or standard
# set of parameters?

standard.mean <- c(standard.amazon$mean,standard.congo$mean,standard.seasia$mean,
                   standard.namerica$mean)#,standard.global$mean)

standard.sd <- c(standard.amazon$sd,standard.congo$sd,standard.seasia$sd,
                   standard.namerica$sd)#,standard.global$sd)


standard.impl.credible <- impl(standard.mean, standard.sd, disc = 0, disc.sd = 0,
             obs = c(obs_amazon, obs_congo, obs_seasia, obs_namerica),
             obs.sd = c(0.05, 0.05, 0.05, 0.05))


# Which bits of parameter space are less implausible than
# The default settings?
# (maybe without the Amazon ...)


test.ix <- plausible.credible$imp.mat[, 2] < standard.impl.credible[2] &
           plausible.credible$imp.mat[, 3] < standard.impl.credible[3] &
           plausible.credible$imp.mat[, 4] <  standard.impl.credible[4]


better.than <- plausible.credible$X.unif[test.ix , ]

dev.new(width = 9, height = 9)
pairs(better.than, panel = dfunc.up, gap = 0, upper.panel = NULL)

dev.new(width = 7, height = 7)
par(mfrow = c(4,2))

for(i in 1:ncol(better.than)){
  hist(better.than[,i], xlim = c(0,1), breaks = seq(from = 0, to = 1, by = 0.05))
}




# Can we produce 2 dimensional maps of places where the model
# discrepancy is *most similar*

discrepancy.diff <- function(X, fit1, fit2, obs1, obs2, n = 100000){

  X.mins <- apply(X,2,min)
  X.maxes <- apply(X,2,max)
  
  X.unif <- samp.unif(n, mins = X.mins, maxes = X.maxes)
  colnames(X.unif) <- colnames(X)
  
  pred1 <- predict(fit1, newdata = X.unif, type = 'UK')
  pred2 <- predict(fit2, newdata = X.unif, type = 'UK')

  disc1 <- pred1$mean - obs1
  disc2 <- pred2$mean - obs2

  ddiff <- disc1 - disc2

  return (list(X.unif = X.unif, disc1 = disc1, disc2 = disc2, ddiff = ddiff))
  
}

dd.amaz.congo <- discrepancy.diff(X.norm, fit1 = fit.amazon, fit2 = fit.congo,
                            obs1 = obs_amazon, obs2 = obs_congo, n = 10000)

dd.seasia.namerica <- discrepancy.diff(X.norm, fit1 = fit.seasia, fit2 = fit.namerica,
                            obs1 = obs_seasia, obs2 = obs_namerica, n = 10000)

# first, plot up the discrepancy in interesting parameter space

zlim = c(-0.7,0.7)

#dev.new(width = 8, height = 8)
pdf(file = '../graphics/paper/discrepancy_parameter_space.pdf', width = 8, height = 8)
par(mfrow = c(2,2), mar = c(5,5,4,5), oma = c(0,0,0,1))

quilt.plot(dd.amaz.congo$X.unif[,3], dd.amaz.congo$X.unif[,7], dd.amaz.congo$disc1,
           main = 'Amazon', zlim = zlim, col = byr, xlab = 'NL0', ylab = 'V_CRIT_ALPHA')

quilt.plot(dd.amaz.congo$X.unif[,3], dd.amaz.congo$X.unif[,7], dd.amaz.congo$disc2,
           main = 'Africa', zlim = zlim, col = byr, xlab = 'NL0', ylab = 'V_CRIT_ALPHA')

quilt.plot(dd.seasia.namerica$X.unif[,3], dd.seasia.namerica$X.unif[,7], dd.seasia.namerica$disc1,
           main = 'SE Asia', zlim = zlim, col = byr, xlab = 'NL0', ylab = 'V_CRIT_ALPHA')

quilt.plot(dd.seasia.namerica$X.unif[,3], dd.seasia.namerica$X.unif[,7], dd.seasia.namerica$disc2,
           main = 'N America', zlim = zlim, col = byr, xlab = 'NL0', ylab = 'V_CRIT_ALPHA')
dev.off()


# Now compare the predictions at the standard values, the ensemble,
# and the observations

#x11(width = 7, height = 7)
pdf(file = '../graphics/paper/fraction_histogram_with_discrepancy_standard.pdf', width = 7, height = 7)
par(mfrow= c(5,1),fg = 'white', las = 1, mar = c(1,2,1,2), oma = c(5,2,0,0), cex.axis = 1.5)

br <- seq(from = 0, to = 1, by = 0.02)

hist.amaz <- hist(AMAZ_MOD_FRAC, plot = FALSE, breaks = br)
hist.congo <- hist(CONGO_MOD_FRAC, plot = FALSE, breaks = br)
hist.namerica <- hist(NAMERICA_MOD_FRAC, plot = FALSE, breaks = br)
hist.seasia <- hist(SEASIA_MOD_FRAC, plot = FALSE, breaks = br)
hist.global <- hist(GLOB_MOD_FRAC, plot = FALSE, breaks = br)

ylim = c(0,20)
ticksize = 0.06

plot(hist.amaz,
     xlim = c(0,1), main = '',xlab = '', ylab = '', col = col.amaz, ylim = ylim, axes = FALSE)
mtext(text = 'Amazon', side = 3, at = 0.1, line = -2, col = 'black')
segments(standard.amazon$mean - standard.amazon$sd,-0.5,
         standard.amazon$mean + standard.amazon$sd, -0.5, col = 'black')
rug(obs_amazon, col = col.amaz, lwd = 5, line = 0, ticksize = ticksize)
rug(standard.amazon$mean, col = 'black', lwd = 5, line = 0)
axis(2, col = 'grey')

plot(hist.seasia,
     main = '',xlab = '', ylab = '', xlim = c(0,1), col = col.seasia, ylim = ylim, axes = FALSE)
mtext(text = 'SE Asia', side = 3, at = 0.1, line = -2, col = 'black')
segments(standard.seasia$mean - standard.seasia$sd,-0.5,
         standard.seasia$mean + standard.seasia$sd, -0.5, col = 'black')
rug(obs_seasia, col = col.seasia, lwd = 5, line = 0)
rug(standard.seasia$mean, col = 'black', lwd = 5, line = 0)
axis(2, col = 'grey')

plot(hist.congo,
      main = '',xlab = '', ylab = '', xlim = c(0,1), col = col.congo, ylim = ylim, axes = FALSE)
mtext(text = 'Central Africa', side = 3, at = 0.1, line = -2, col = 'black')
rug(obs_congo, col = col.congo, lwd = 5, line = 0)
rug(standard.congo$mean, col = 'black', lwd = 5, line = 0)
segments(standard.congo$mean - standard.congo$sd,-0.5,
         standard.congo$mean + standard.congo$sd, -0.5, col = 'black')
axis(2, col = 'grey')

plot(hist.namerica,
     main = '',xlab = '', ylab = '', xlim = c(0,1), col = col.namerica, ylim = ylim, axes = FALSE)
mtext(text = 'N America', side = 3, at = 0.1, line = -2, col = 'black')
segments(standard.namerica$mean - standard.namerica$sd,-0.5,
         standard.namerica$mean + standard.namerica$sd, -0.5, col = 'black')
rug(obs_namerica, col = col.namerica, lwd = 5, line = 0)
rug(standard.namerica$mean, col = 'black', lwd = 5, line = 0)
axis(2, col = 'grey')

plot(hist.global,
     main = '',xlab = '', ylab = '', xlim = c(0,1), col = col.global, ylim = ylim, axes = FALSE)
mtext(text = 'Global', side = 3, at = 0.1, line = -2, col = 'black')
mtext(text = 'Mean forest fraction', side = 1, adj = 0.5, line = 4, col = 'black')
segments(standard.global$mean - standard.global$sd,-0.5,
         standard.global$mean + standard.global$sd, -0.5, col = 'black')
rug(obs_glob, col = col.global, lwd = 5, line = 0)
rug(standard.global$mean, col = 'black', lwd = 5, line = 0)
axis(1, col = 'grey', line = 1)
axis(2, col = 'grey')

par(fg = 'black')
legend('topright', legend = c('standard inputs', 'observation'),
       pch = 19, col = c('black', col.global), lwd = c(1,NA), bty = 'n', lty = 'solid', cex = 1.5)

dev.off()


# -------------------------------------------------------------------------------
# Change the mean of the discrepancy to be the difference between
# the obs and the emulator at the default setting
# -------------------------------------------------------------------------------
plausible.disc.amazon <- multiple.inputs.set(X = X.norm,
                                      Y = Y.all,
                                      thres = 3,
                                      obs = c(obs_amazon,obs_seasia, obs_congo,obs_namerica),
                                      obs.sd = cbind(rep(0.05,100), rep(0.05,100), rep(0.05,100), rep(0.05,100) ), 
                                      disc = cbind(rep(disc.standard.amazon,100), rep(0,100), rep(0.0,100), rep(0,100) ),
                                      disc.sd = cbind(rep(0.03,100), rep(0.03,100),rep(0.03,100),rep(0.03,100)),
                                      n = 300000,
                                      abt = FALSE)

(dim(plausible.disc.amazon$X.out)[1]) / 300000


#dev.new(width = 7, height = 7)
pdf(file = '../graphics/paper/input_frequency_marginal.pdf', width = 7, height = 7)
par(mfrow = c(4,2), fg = 'white', mar = c(4,5,2,1), cex.lab = 1.5, oma = c(0,3,0,0))

for(i in 1:7){

hist(plausible.disc.amazon$X.out[, i], xlim = c(0,1), breaks = seq(from = 0, to = 1, by = 0.1), axes = FALSE,, col = 'grey', xlab = colnames(plausible.disc.amazon$X.out)[i], main = '', ylab = '')
axis(1, col = 'black')
axis(2, col = 'black')

}

mtext('Relative frequency of NROY input points', side = 2,line = 1, outer = TRUE, las = 0, col = 'black', cex  = 1.5)
dev.off()



pdf(width = 7, height = 7, file = '../graphics/paper/plausible_disc_input_space.pdf')
#dev.new(width = 10, height = 10)
pairs(plausible.disc.amazon$X.out, panel = dfunc.up, gap = 0, upper.panel = NULL)
dev.off()

# What does an ensemble of output look like that is sampled from this input space


# Predict forest fraction in the credible (with discrepancy) NROY space
pred.amaz.disc <- predict(fit.amazon, newdata = plausible.disc.amazon$X.out, type = 'UK')
pred.congo.disc <- predict(fit.congo, newdata = plausible.disc.amazon$X.out, type = 'UK')
pred.namerica.disc <- predict(fit.namerica, newdata = plausible.disc.amazon$X.out, type = 'UK')
pred.seasia.disc <- predict(fit.seasia, newdata = plausible.disc.amazon$X.out, type = 'UK')
pred.global.disc <- predict(fit.global, newdata = plausible.disc.amazon$X.out, type = 'UK')



hbreaks <- seq(from = 0, to = 1, by = 0.02)

pdf(file = '../graphics/paper/credible_NROY_hists_disc.pdf', width = 6, height = 8)
#dev.new(width=6, height=8)
par(mfrow= c(5,1),fg = 'white', las = 1, mar = c(3,1,2,0), oma = c(3,5,0,1), cex.axis = 1.3)
hist(pred.amaz.disc$mean, xlim=c(0,1), breaks=hbreaks, prob=TRUE,
     col=col.amaz, axes=FALSE,
     main='', xlab='', ylab='')
mtext(text='Amazon', side=3, line=-0.7, col='black', adj = 0.03, cex = 1.2)
axis(1, col='grey', line=1)
axis(2, col = 'grey')
par(xpd = NA)
rug(obs_amazon, col = col.amaz, lwd=5, line = 1.2)

hist(pred.congo.disc$mean, xlim=c(0,1), breaks=hbreaks, prob=TRUE,
     col=col.congo, axes=FALSE,
     main='', xlab='', ylab='')
mtext(text='Congo', side=3, line=-0.7, col='black', adj = 0.03, cex = 1.2)
axis(1, col='grey', line=1)
axis(2, col='grey')
par(xpd=NA)
rug(obs_congo, col = col.congo, lwd=5, line = 1.2)

hist(pred.namerica.disc$mean, xlim=c(0,1), breaks=hbreaks, prob=TRUE,
     col=col.namerica, axes=FALSE,
     main='', xlab='', ylab='')
mtext(text='N America', side=3, line=-0.7, col='black', adj = 0.03, cex = 1.2)
axis(1, col='grey', line=1)
axis(2, col='grey')
par(xpd=NA)
rug(obs_namerica, col=col.namerica, lwd=5, line=1.2)

hist(pred.seasia.disc$mean, xlim = c(0,1), breaks = hbreaks, prob=TRUE,
     col = col.seasia, axes = FALSE,
     main = '', xlab = '', ylab = '')
mtext(text='SE Asia', side=3, line=-0.7, col='black', adj = 0.03, cex = 1.2)
axis(1, col = 'grey', line = 1)
axis(2, col = 'grey')
par(xpd = NA)
rug(obs_seasia, col = col.seasia, lwd = 5, line = 1.2)

hist(pred.global.disc$mean, xlim = c(0,1), breaks = hbreaks, prob=TRUE,
     col = col.global, axes = FALSE,
     main = '', xlab = '', ylab = '')
mtext(text='Global', side=3, line=-0.7, col='black', adj = 0.03, cex = 1.2)
axis(1, col = 'grey', line = 1)
axis(2, col = 'grey')
par(xpd = NA)
rug(obs_glob, col = col.global, lwd = 5, line = 1.2)

mtext(outer=TRUE, side=2, text='Density', line=2.5, col='black',  las=0, cex=1.3) 
mtext(outer=TRUE, side=1, text='Forest Fraction', line=2, col='black', fg = 'black', las=0, cex=1.3)

dev.off()


#stop()

# what is the relative proportion of emulator error to othr types of error
# in the implausibility calulation?

# start with a map of emulator error

em.unc.map <- function(X,y, n = 100000){

  X.mins <- apply(X,2,min)
  X.maxes <- apply(X,2,max)
  
  X.unif <- samp.unif(n, mins = X.mins, maxes = X.maxes)
  colnames(X.unif) <- colnames(X)

  fit <- km(~., design = X, response = y, control = list(trace = FALSE))
            
  pred <- predict(fit, newdata = X.unif, type = 'UK')

            
 # pred.impl <- impl(em = pred$mean, em.sd = pred$sd,
 #                   disc = disc, obs = obs, disc.sd = disc.sd, obs.sd = obs.sd)

  return (list(X.unif = X.unif, pred = pred))
  
}


test <- em.unc.map(X = X.norm, y = model_amazon)



pdf(file = '../graphics/test.pdf', width = 8, height = 8)

quilt.plot(test$X.unif[,3], test$X.unif[,7], test$pred$sd, main = 'Amazon', col = byr, xlab = 'NL0', ylab = 'V_CRIT_ALPHA')

dev.off()





  


dd.amaz.congo <- discrepancy.diff(X.norm, fit1 = fit.amazon, fit2 = fit.congo,
                            obs1 = obs_amazon, obs2 = obs_congo, n = 10000)

dd.seasia.namerica <- discrepancy.diff(X.norm, fit1 = fit.seasia, fit2 = fit.namerica,
                            obs1 = obs_seasia, obs2 = obs_namerica, n = 10000)

# first, plot up the discrepancy in interesting parameter space

zlim = c(-0.7,0.7)

#dev.new(width = 8, height = 8)
#pdf(file = '../graphics/discrepancy_parameter_space.pdf', width = 8, height = 8)
#par(mfrow = c(2,2), mar = c(5,5,4,5), oma = c(0,0,0,1))

#quilt.plot(dd.amaz.congo$X.unif[,3], dd.amaz.congo$X.unif[,7], dd.amaz.congo$disc1,
#           main = 'Amazon', zlim = zlim, col = byr, xlab = 'NL0', ylab = 'V_CRIT_ALPHA')



uncfrac <- function(em, em.sd, disc, disc.sd, obs, obs.sd){
  # implausibility function
  # All uncertainties should be expressed as a single standard deviation.

  # What fraction is due 

  impl.squared <-  (em - disc - obs)^2 / (em.sd^2 + disc.sd^2 + obs.sd^2)

  impl <- sqrt(impl.squared)

  impl

  
}



em.unc.map <- function(X, em, obs){

    
  X.mins <- apply(X,2,min)
  X.maxes <- apply(X,2,max)
  
  X.unif <- samp.unif(n, mins = X.mins, maxes = X.maxes)
  colnames(X.unif) <- colnames(X)

  # a measure of the relative size of the emulator uncertainty to model discrepancy 

}




#save.image('../data/best_X_200140929.Rdata')






#byr[4:11]
stop()

# range across the ensemble

frac.max <- apply(bl.frac.ens.nona, 2, max)

frac.min <- apply(bl.frac.ens.nona, 2, min)

frac.range <- frac.max - frac.min

dev.new(width = 7, height = 12)
par(mfrow = c(3,1))

image.plot(longs, rev(lats), remap.famous(frac.max, longs, lats), col = byr,
           zlim = c(0,1), axes = FALSE)

image.plot(longs, rev(lats), remap.famous(frac.min, longs, lats), col = byr,
           zlim = c(0,1), axes = FALSE)

image.plot(longs, rev(lats), remap.famous(frac.range, longs, lats), col = byr,
           zlim = c(0,1), axes = FALSE)


dev.new(width = 7, height = 5)
image.plot(longs, rev(lats), remap.famous((amazon.mean - congo.mean)/frac.range, longs, lats), col = byr,
           zlim = c(-.1,.1), axes = FALSE)

dev.new()
hist((amazon.mean - congo.mean)/frac.range)
