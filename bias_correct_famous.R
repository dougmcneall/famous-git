# Using temperature and precip data to bias correst FAMOUS,
# and find a good set of input parameters.



# -------------------------------------------------------
# 0. Packages, functions and data
# -------------------------------------------------------

source('famous_common.R')

# -------------------------------------------------------
# 1. Combine the input parameters and tropical forest
# fraction data into a single data matrix
# -------------------------------------------------------

amazon.dat <- cbind(X, temps$Amazon, precips$Amazon, AMAZ_MOD_FRAC)
colnames(amazon.dat) <- c(colnames(X), 'TEMP', 'PRECIP', 'FRAC')

congo.dat <- cbind(X, temps$Congo, precips$Congo, CONGO_MOD_FRAC)
colnames(congo.dat) <- c(colnames(X), 'TEMP', 'PRECIP', 'FRAC')

seasia.dat <- cbind(X, temps$SE, precips$SEAsia, SEASIA_MOD_FRAC)
colnames(seasia.dat) <- c(colnames(X), 'TEMP', 'PRECIP', 'FRAC')

tropics.dat <- rbind(amazon.dat, congo.dat, seasia.dat)

X.tropics.norm <- normalize(tropics.dat[, 1:9])
Y.tropics <- tropics.dat[ ,10]

tropics.fit <-  km(~.,  design=X.tropics.norm, response=Y.tropics)


# Train the emulator using all of the data,
# Then see what the Amazon would look like if it had sensible T and P.

# create a vector of default parameters and amazon obs

# normalize amazon obs
tp.amaz.norm <- normalize(
                          matrix(c(temps_obs$Amazon+273.15, precips_obs$Amazon),nrow=1),
                          wrt=tropics.dat[, 8:9]
                          )
colnames(tp.amaz.norm) <- c('TEMP', 'PRECIP')

tp.congo.norm <- normalize(
                           matrix(c(temps_obs$Congo+273.15, precips_obs$Congo), nrow=1),
                           wrt=tropics.dat[, 8:9]
                           )
colnames(tp.congo.norm) <- c('TEMP', 'PRECIP')

tp.seasia.norm <- normalize(
                            matrix(c(temps_obs$SEAsia+273.15, precips_obs$SEAsia), nrow=1),
                            wrt=tropics.dat[, 8:9]
                            )
colnames(tp.seasia.norm) <- c('TEMP', 'PRECIP')

amaz.x  <- cbind(X.stan.norm, tp.amaz.norm)
congo.x <- cbind(X.stan.norm, tp.congo.norm)
seasia.x <- cbind(X.stan.norm, tp.seasia.norm)

pred.amaz.bc <- predict(tropics.fit, newdata=amaz.x, type='UK')
pred.congo.bc <- predict(tropics.fit, newdata=congo.x, type='UK')
pred.seasia.bc <- predict(tropics.fit, newdata=seasia.x, type='UK')


fit.amazon <- km(~., design=X.norm, response=AMAZ_MOD_FRAC)
fit.congo <- km(~., design=X.norm, response=CONGO_MOD_FRAC)
fit.namerica <- km(~., design=X.norm, response=NAMERICA_MOD_FRAC)
fit.seasia <- km(~., design=X.norm, response=SEASIA_MOD_FRAC)
fit.global <- km(~., design=X.norm, response=GLOB_MOD_FRAC)

standard.amazon <- predict(fit.amazon, newdata=X.stan.norm, type='UK')
standard.congo <- predict(fit.congo, newdata=X.stan.norm, type='UK')
standard.seasia <- predict(fit.seasia, newdata=X.stan.norm, type='UK')
standard.namerica <- predict(fit.namerica, newdata=X.stan.norm, type='UK')
standard.global <- predict(fit.global, newdata=X.stan.norm, type='UK')


# It looks like bias correction via T and P improves the Amazon,
# slightly worsens the Congo, and very slightly improves SE Asia. 
obs_amazon - standard.amazon$mean
obs_amazon - pred.amaz.bc$mean

obs_congo - standard.congo$mean
obs_congo - pred.congo.bc$mean

obs_seasia - standard.seasia$mean
obs_seasia - pred.seasia.bc$mean


# ---------------------------------------------------------
# How much does including temperature and precipiation
# as inputs improve the emulator?
# ---------------------------------------------------------

loo.amazon <- leaveOneOut.km(fit.amazon, type='UK')
loo.congo <- leaveOneOut.km(fit.congo, type='UK')
loo.seasia <- leaveOneOut.km(fit.seasia, type='UK')
loo.tropics <- leaveOneOut.km(tropics.fit,type='UK')

# Mean LOO abs. error when fit using all parameters (inc. temp & precip)
mean(abs(loo.tropics$mean - Y.tropics))

mean(abs(loo.amazon$mean - AMAZ_MOD_FRAC))
mean(abs(loo.congo$mean - CONGO_MOD_FRAC))
mean(abs(loo.seasia$mean - SEASIA_MOD_FRAC))

fit.diff <- c(loo.amazon$mean, loo.congo$mean, loo.seasia$mean) - c(AMAZ_MOD_FRAC, CONGO_MOD_FRAC, SEASIA_MOD_FRAC)

# Mean LOO abs. error when fit using only land surface parameters
mean(abs(fit.diff))

# Compare the leave-one-out error of the emulator fit
# using all parametrs (inc. temp and precip) and land surface only
breaks <- seq(from=0, to=0.3, by=0.01)
col.lsp <- adjustcolor('red', alpha=0.4)
col.all <- adjustcolor('black', alpha=0.4)

#dev.new(width=5, height=7)
pdf(file='../graphics/all_vs_lsp_fit_error.pdf', width=5, height=7)
par(mfrow = c(2,1), fg = 'white', las = 1)

hist(abs(loo.tropics$mean - Y.tropics),
     xlab='emulator error (absolute)',
     main = '',
     xlim=c(0, 0.25),
     breaks=breaks,
     col=col.all,
     axes=FALSE)

hist(abs(fit.diff),
     xlim=c(0, 0.3), breaks=breaks, add=TRUE, col=col.lsp)

legend('topright', legend=c('all','land surface'), fill=c(col.all, col.lsp), text.col='black', border='white')
axis(1, col='darkgrey')
axis(2, col='darkgrey')

plot(Y.tropics, c(loo.amazon$mean, loo.congo$mean, loo.seasia$mean),
     col=col.lsp, pch=20, axes=FALSE, xlab='exact', ylab='predicted')
points(Y.tropics, loo.tropics$mean, col = col.all, pch = 20)
legend('topleft',legend=c('all','land surface'),col=c(col.all, col.lsp), pch=c(20,20), text.col='black')
abline(0,1)
axis(1, col='darkgrey')
axis(2, col='darkgrey')
dev.off()


# ---------------------------------------------------------
# BIAS CORRECTION SECTION
# If we bias correct everything using T and P,
# does that bring us closer to the observations in all cases?
# Plot the standard and bias corrected
# ---------------------------------------------------------

dev.new(width=7, height=5)
#pdf(width = 7, height = 5, file='../graphics/bias_corrected_fractions.pdf')
par(las=1)

plot(c(1,2,3), c(obs_amazon, obs_congo, obs_seasia), xlim=c(0.5,3.5), ylim=c(0,1), pch=19,
col = c(col.amaz, col.congo, col.seasia), cex=1.5, axes=FALSE, xlab='', ylab='Forest fraction')

axis(1, labels = c('Amazon', 'Africa', 'SE Asia'), at = c(1.1,2.1,3.1))
axis(2)

points(c(1.1,2.1,3.1), c(standard.amazon$mean, standard.congo$mean,standard.seasia$mean), pch=19)

segments(1.1, standard.amazon$mean - standard.amazon$sd,
         1.1, standard.amazon$mean + standard.amazon$sd, col='black')

segments(2.1, standard.congo$mean - standard.congo$sd,
         2.1, standard.congo$mean + standard.congo$sd, col='black')

segments(3.1, standard.seasia$mean - standard.seasia$sd,
         3.1, standard.seasia$mean + standard.seasia$sd, col='black')

points(c(1.2,2.2,3.2),c(pred.amaz.bc$mean, pred.congo.bc$mean, pred.seasia.bc$mean),  col='red', pch=19)

segments(1.2, pred.amaz.bc$mean - pred.amaz.bc$sd,
         1.2, pred.amaz.bc$mean + standard.amazon$sd, col='red')

segments(2.2, pred.congo.bc$mean - standard.congo$sd,
         2.2, pred.congo.bc$mean + standard.congo$sd, col='red')

segments(3.2, pred.seasia.bc$mean - pred.seasia.bc$sd,
         3.2, pred.seasia.bc$mean + pred.seasia.bc$sd, col='red')

text(1, obs_amazon, 'observation', pos=2, col='grey', cex=0.9)
text(1.1, standard.amazon$mean, 'default\nparameters', pos=4, cex=0.9, col='grey')
text(1.2, pred.amaz.bc$mean, 'bias\ncorrected', pos=4, col='grey', cex=0.9)
#abline(v = c(1.5,2.5), col = 'grey', lty = 'dashed')
#abline(h = c(0.2,0.4, 0.6, 0.8), col = 'grey', lty = 'dashed')
#dev.off()


# What is the implausibility at the default parameters?
amaz.bc.imp <- impl(em=pred.amaz.bc$mean,
                    em.sd=pred.amaz.bc$sd,
                    disc=0,
                    disc.sd=0.03,
                    obs=obs_amazon,
                    obs.sd=0.05
                    )
congo.bc.imp <- impl(em=pred.congo.bc$mean,
                    em.sd=pred.congo.bc$sd,
                    disc=0,
                    disc.sd=0.03,
                    obs=obs_congo,
                    obs.sd=0.05
                    )
seasia.bc.imp <- impl(em=pred.seasia.bc$mean,
                    em.sd=pred.seasia.bc$sd,
                    disc=0,
                    disc.sd=0.03,
                    obs=obs_seasia,
                    obs.sd=0.05
                    ) 



#in.ix <-  X.tropics.norm[ ,'TEMP'] < (tp.amaz.norm[1] + 0.1) &
#          X.tropics.norm[ ,'TEMP'] > (tp.amaz.norm[1] - 0.1) &
#          X.tropics.norm[ ,'PRECIP'] < (tp.amaz.norm[2] + 0.1) &
#          X.tropics.norm[ ,'PRECIP'] > (tp.amaz.norm[2] - 0.1)



# Find the set of plausible inputs, when temperature and precip are included in the inputs
plausible.amazon.bc <- inputs.set(X = X.tropics.norm, y = Y.tropics,thres = 3,
                   obs = obs_amazon,
                   obs.sd = 0,
                   disc = 0,
                   disc.sd = 0.01,
                   n = 100000,
                   abt = FALSE)


hbreaks <- seq(from = 0, to = 1, by = 0.02)

#pdf(file = '../graphics/credible_NROY_hists.pdf', width = 6, height = 8)
dev.new(width=6, height=8)
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

#dev.off()


# Here is the emulated surface
#pdf(file = '../graphics/emulated_fraction_vs_temp_precip.pdf',width = 7, height = 7)
dev.new(width = 7, height  7)
quilt.plot(plausible.amazon.bc$X.unif[,8], plausible.amazon.bc$X.unif[, 9], plausible.amazon.bc$pred$mean, col = byr[2:10], xlab = 'Normalised regional temperature', ylab = 'Normalised regional precipitation', legend.args = list(text = "forest\nfraction",col="black", cex=1.2, side=3, line=1))

points(X.tropics.norm[1:100,8], X.tropics.norm[1:100,9], col = 'black', bg = col.amaz, pch = 21)
points(X.tropics.norm[101:200,8], X.tropics.norm[101:200,9], col = 'black', bg = col.congo, pch = 21)
points(X.tropics.norm[201:300,8], X.tropics.norm[201:300,9], col = 'black', bg = col.seasia, pch = 21)
points(tp.amaz.norm, col = 'black', pch = 21, cex = 2, bg = col.amaz, lwd = 2.5)
points(tp.congo.norm, col = 'black', pch = 21, cex = 2, bg = col.congo, lwd = 2.5)
points(tp.seasia.norm, col = 'black', pch = 21, cex = 2, bg = col.seasia, lwd = 2.5)

text(tp.amaz.norm, 'Amazon', pos = 4, font = 2)
text(tp.congo.norm, 'Central Africa', pos = 4, font = 2)
text(tp.seasia.norm, 'SE Asia', pos = 4, font = 2)

#dev.off()




pdf(width = 7, height = 7, file = '../graphics/best_inputs_amazon_bc.pdf')
pairs(plausible.amazon.bc$X.out, panel = dfunc.up, gap = 0, upper.panel = NULL)
dev.off()

# Find the set of inputs that also doesn't stray too far from the
# Amazon observed (not modelled) temperature and precip.
in.ix <-  plausible.amazon.bc$X.out[ ,'TEMP'] < (tp.amaz.norm[1] + 0.1) &
          plausible.amazon.bc$X.out[ ,'TEMP'] > (tp.amaz.norm[1] - 0.1) &
          plausible.amazon.bc$X.out[ ,'PRECIP'] < (tp.amaz.norm[2] + 0.1) &
          plausible.amazon.bc$X.out[ ,'PRECIP'] > (tp.amaz.norm[2] - 0.1)



dev.new(width = 10, height = 10)
pairs(plausible.amazon.bc$X.out[in.ix, ] , panel = dfunc.up, gap = 0, upper.panel = NULL, xlim = c(0,1), ylim = c(0,1))


# Sensitivity analysis, including temperature and precipitation
library(sensitivity)

n <- 21
X.oat <- oaat.design(X.tropics.norm, n = n, hold = c(X.stan.norm,tp.amaz.norm))
colnames(X.oat) <- colnames(X.tropics.norm)

fit.sens <- km(~.,design = X.tropics.norm, response = Y.tropics)

pred.sens <- predict(fit.sens, newdata = X.oat, type = 'UK')

col.chosen = col.amaz
col.transp <- adjustcolor(col.chosen, alpha = 0.5)


dev.new(width = 7, height = 6)

#pdf(width = 7, height = 6, file = '../graphics/sensitivity_TP_amazon.pdf') 
par(mfrow = c(2,5), las = 1, mar = c(5,0.5,2,0.5), oma = c(0,5,0,0), fg = 'grey')

for(i in 1: ncol(X.tropics.norm)){

  ix <- seq(from = ((i*n) - (n-1)), to =  (i*n), by = 1)
  
  #  plot(X.tropics.norm[, i], Y.tropics, xlab = colnames(X.tropics.norm)[i],
  #     col = NULL, pty = 'n', ylim = c(0,1), bty = 'n',
  #     ylab = '', cex.lab = 1.5, cex.axis = 1.5, axes = FALSE)
  
  plot(X.oat[ix, i], pred.sens$mean[ix], ylim = c(0,1), xlab = colnames(X.oat)[i], type = 'n', axes = FALSE)
  axis(1)
  if (i==1 | i==6 ) {axis(2)
                     mtext(side = 2, line = 3.5, text = 'FOREST FRACTION', las = 0, col = 'black')
                    }
  
  
  polygon(x = c(X.oat[ix, i], rev(X.oat[ix, i])),
          y = c( (pred.sens$mean[ix] - pred.sens$sd[ix]), rev(pred.sens$mean[ix] + pred.sens$sd[ix])),
          col = col.transp, border = col.transp)
            

  lines(X.oat[ix, i], pred.sens$mean[ix], ylim = c(0,1), xlab = colnames(X.oat)[i], col = col.chosen )
    
}

#dev.off()

dev.new()
plot(tropics.dat$FRAC, tropics.dat$PRECIP,
     col = c(rep(col.amaz, 100), rep(col.congo, 100), rep(col.seasia, 100)),
     pch = 19)

dev.new()
plot(tropics.dat$FRAC, tropics.dat$TEMP - 273.15,
     col = c(rep(col.amaz, 100), rep(col.congo, 100), rep(col.seasia, 100)),
     pch = 19)


dev.new()
plot(tropics.dat$TEMP - 273.15,tropics.dat$FRAC,
     col = c(rep(col.amaz, 100), rep(col.congo, 100), rep(col.seasia, 100)),
     pch = 19)



# Hold everything still - all the parameters etc, and plot the temperature against fraction

dev.new()
plot(tropics.dat$FRAC, tropics.dat$TEMP - 273.15,
     col = c(rep(col.amaz, 100), rep(col.congo, 100), rep(col.seasia, 100)),
     pch = 19)

dev.new()
plot(tropics.dat$FRAC, tropics.dat$TEMP - 273.15,
     col = c(rep(col.amaz, 100), rep(col.congo, 100), rep(col.seasia, 100)),
     pch = 19)


dev.new()
i <- 8
ix <- seq(from = ((i*n) - (n-1)), to =  (i*n), by = 1)
plot( pred.sens$mean[ix],X.oat[ix, i])

