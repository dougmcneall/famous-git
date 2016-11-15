#temp_precip_famous.R

# experiments with aggregated temperature and precipitation in FAMOUS

# -------------------------------------------------------
# 0. Packages, functions and data
# -------------------------------------------------------

source('famous_common.R')

# -------------------------------------------------------
# 1. Basic plotting of temperature and precipitation
# -------------------------------------------------------


# How do parameters impact on temperature?
x11(width = 10, height = 10)
pairs(cbind(params[,4:10],  temps[, 2:5]))


# How do parameters impact on precipitation?
x11(width = 10, height = 10)
pairs(cbind(params[,4:10],  precips[, 2:5]))


# How do local temparature and precip
# impact on forest fraction?

x11(width = 12, height = 6)

#pdf(width = 12, height = 6, file = '../graphics/temp_precip_frac.pdf')
par(mfcol = c(2,4), las = 1, cex.lab = 1.3)

plot(temps$Global, full_frac$GLOB_MOD_FRAC,
     xlab = 'Temperature', ylab = 'Forest fraction', main = 'GLOBAL')

plot(precips$Global, full_frac$GLOB_MOD_FRAC,
     xlab = 'Precipitation', ylab = 'Forest fraction')

plot(temps$Amazon, full_frac$AMAZ_MOD_FRAC,
     xlab = 'Temperature', ylab = 'Forest fraction', main = 'AMAZON')

plot(precips$Amazon, full_frac$AMAZ_MOD_FRAC,
     xlab = 'Precipitation', ylab = 'Forest fraction')

plot(temps$Congo, full_frac$CONGO_MOD_FRAC,
     xlab = 'Temperature', ylab = 'Forest fraction', main = 'CONGO')

plot(precips$Congo, full_frac$CONGO_MOD_FRAC,
     xlab = 'Precipitation', ylab = 'Forest fraction')

plot(temps$SEAsia, full_frac$SEASIA_MOD_FRAC,
     xlab = 'Temperature', ylab = 'Forest fraction', main = 'SE ASIA')

plot(precips$SEAsia, full_frac$SEASIA_MOD_FRAC,
     xlab = 'Precipitation', ylab = 'Forest fraction')

dev.off()


# How do local temparature and precip
# impact on forest fraction? (Colors, single graphs)
#pdf(file = '../graphics/precip_temp_frac_rug.pdf', width = 7, height = 9)
x11(width = 7, height = 9)
par(mfrow = c(2,1), las = 1, fg = 'grey')

plot(temps$Global - 273.15, full_frac$GLOB_MOD_FRAC,
     xlab = expression(paste('Temperature (',degree,'C)')),
     ylab = 'Forest fraction', main = '',
     xlim = c(13,32.5), ylim= c(0,0.9), col = col.global, pch = pch.global)

points(temps$Amazon - 273.15, full_frac$AMAZ_MOD_FRAC,
       col = col.amaz, pch = pch.amaz)

points(temps$Congo - 273.15, full_frac$CONGO_MOD_FRAC,
     col = col.congo, pch = pch.congo)

points(temps$SEAsia - 273.15, full_frac$SEASIA_MOD_FRAC,
     col = col.seasia, pch = pch.seasia, cex = 1.2)

legend('topleft', legend = c('Global', 'Amazon', 'Congo', 'SE Asia'),
       col = c(col.global, col.amaz, col.congo, col.seasia),
       pch = c(pch.global, pch.amaz, pch.congo, pch.seasia), bty = 'n', text.col = 'black')


rug(temps_obs$Global, col = col.global, lwd = 3)
rug(temps_obs$Amazon, col = col.amaz, lwd = 3)
rug(temps_obs$Congo, col = col.congo, lwd = 3)
rug(temps_obs$SEAsia, col = col.seasia, lwd = 3)

plot(precips$Global, full_frac$GLOB_MOD_FRAC,
     xlab = expression(paste('Precipitation (kgm'^-2,'s'^-1,')')),
     ylab = 'Forest fraction', main = '',
     ylim = c(0,0.9),xlim = c(3e-5, 1e-4), col = col.global, pch = pch.global  )

points(precips$Amazon, full_frac$AMAZ_MOD_FRAC, col = col.amaz, pch = pch.amaz )
points(precips$Congo, full_frac$CONGO_MOD_FRAC,col = col.congo, pch = pch.congo )
points(precips$SEAsia, full_frac$SEASIA_MOD_FRAC,  col = col.seasia, pch = pch.seasia, cex = 1.2)

rug(precips_obs$Global, col = col.global, lwd = 3)
rug(precips_obs$Amazon, col = col.amaz, lwd = 3)
rug(precips_obs$Congo, col = col.congo, lwd = 3)
rug(precips_obs$SEAsia, col = col.seasia, lwd = 3)

#dev.off()



# Where are the observations & models in temp/precip space?
x11()
#pdf(width = 7, height = 7, file = '../graphics/temp_precip_space.pdf')
par(mar = c(5,5,3,2), fg = 'grey')
plot(temps$Global - 273.15, precips$Global,
     xlim = c(13,32.5), ylim = c(3e-5, 1e-4),
     pch = pch.global, col = col.global,
     xlab = expression(paste('Temperature (',degree,'C)')),
     ylab = expression(paste('Precipitation (kgm'^-2,'s'^-1,')')),
     main = ''
     )

points(temps$Amazon - 273.15, precips$Amazon, pch = pch.amaz, col = col.amaz)
points(temps$Congo - 273.15, precips$Congo, pch = pch.congo, col = col.congo)
points(temps$SEAsia - 273.15, precips$SEAsia, pch = pch.seasia, col = col.seasia)


points(temps_obs$Global, precips_obs$Global, pch = pch.global, col = col.global, cex = 2, lwd = 3)
points(temps_obs$Amazon, precips_obs$Amazon, pch = pch.amaz, col = col.amaz, cex = 2, lwd = 3)
points(temps_obs$Congo, precips_obs$Congo, pch = pch.congo, col = col.congo, cex = 2, lwd = 3)
points(temps_obs$SEAsia, precips_obs$SEAsia, pch = pch.seasia, col = col.seasia, cex = 2.2, lwd = 3)

legend('topleft', legend = c('Global', 'Amazon', 'Congo', 'SE Asia', 'Model', 'Observation'),
       col = c(col.global, col.amaz, col.congo, col.seasia, col.global, col.global),
       pch = c(pch.global,pch.amaz,pch.congo,pch.seasia,pch.global,pch.global), pt.cex = c(1,1,1,1,1,2),pt.lwd = c(1,1,1,1,1,3), bty = 'n', text.col = 'black')

#dev.off()


# Temp, precip and fraction quilt plot
library(fields)

temps.all <- c(temps$Global, temps$Amazon, temps$Congo, temps$SEAsia) - 273.15
precips.all<- c(precips$Global, precips$Amazon, precips$Congo,precips$SEAsia)
fracs.all <- c(GLOB_MOD_FRAC, AMAZ_MOD_FRAC, CONGO_MOD_FRAC, SEASIA_MOD_FRAC)

x11(height = 7, width = 7.5)
#pdf(width = 7.5, height = 7, file = '../graphics/temp_precip_frac_quilt.pdf')
par(mar = c(5,6,3,2), fg = 'grey')
quilt.plot(x = temps.all,
           y = precips.all,
           z = fracs.all,
           col = byr, xlim = c(13,32.5), ylim = c(3e-5, 1e-4), fg = 'grey',
           xlab = expression(paste('Temperature (',degree,'C)')),
           ylab = expression(paste('Precipitation (kgm'^-2,'s'^-1,')')),
           legend.args = list(text = "forest\nfraction",
             col="black", cex=1.2, side=3, line=1))
           )

#dev.off()


pairs(cbind(params[,4:10],  precips[, 2:5]))

pairs(temps[, 2:5]), )


plot(temps$Global, full_frac$GLOB_MOD_FRAC)
plot(precips$Global, full_frac$GLOB_MOD_FRAC)

plot(temps$Amazon, full_frac$AMAZ_MOD_FRAC)
plot(precips$Amazon, full_frac$AMAZ_MOD_FRAC)


crit <- cbind(AMAZ_MOD_FRAC,
              Temp_AMAZON= temps$Amazon,
              Precip_AMAZON = precips$Amazon,
              V_CRIT_ALPHA = params$V_CRIT_ALPHA
              )
x11()
#pdf(file = '../graphics/AmazonTempPrecipPairs.pdf')
pairs(crit, lower.panel = NULL)
#dev.off()

crit <- cbind(CONGO_MOD_FRAC,
              Temp_CONGO = temps$Congo,
              Precip_CONGO = precips$Congo,
              V_CRIT_ALPHA = params$V_CRIT_ALPHA
              )
x11()
#pdf(file = '../graphics/CongoTempPrecipPairs.pdf')
pairs(crit, lower.panel = NULL)
#dev.off()

crit <- cbind(SEASIA_MOD_FRAC,
              Temp_SEASIA = temps$SEAsia,
              Precip_SEASIA = precips$SEAsia,
              V_CRIT_ALPHA = params$V_CRIT_ALPHA
              )
#pdf(file = '../graphics/SEAsiaTempPrecipPairs.pdf')
x11()
pairs(crit, lower.panel = NULL)
#dev.off()


#Precip  = precips$Amazon,


#pdf(file = '../graphics/pairs_temp_vcrit_amazon.pdf')
x11()
crit <- cbind(AMAZ_MOD_FRAC, Temp_AMZON= temps$Amazon, V_CRIT_ALPHA = params$V_CRIT_ALPHA)
pairs(crit)
#dev.off()

#pdf(file = '../graphics/pairs_temp_vcrit_congo.pdf')
x11()
crit <- cbind(CONGO_MOD_FRAC, Temp_CONGO = temps$Congo, V_CRIT_ALPHA = params$V_CRIT_ALPHA)
pairs(crit)
#dev.off()

#pdf(file = '../graphics/pairs_temp_vcrit_congo.pdf')
x11()
crit <- cbind(SEAISA_MOD_FRAC, Temp_SEASIA = temps$Congo, V_CRIT_ALPHA = params$V_CRIT_ALPHA)
pairs(crit)
#dev.off()


##-----------------------------------------------------
## Attempting to bias correct by including temperature
## and precip as inputs 
## ----------------------------------------------------


#test against the standard history match


fit.amazon <- km(~.,  design = X.norm, response = AMAZ_MOD_FRAC)
plausible.amazon <- inputs.set(X = X.norm, y = AMAZ_MOD_FRAC,thres = 3,
                   obs = obs_amazon,
                   obs.sd = 0,
                   disc = 0,
                   disc.sd = 0.01,
                   n = 100000,
                   abt = FALSE)
pred.amazon <- predict(fit.amazon, newdata = plausible.amazon$X.out, type = 'UK')



Xdash.amazon <- cbind(params_beta[, 4:10], temps = temps$Amazon, precips = precips$Amazon)
#Xdash.amazon.norm <- normalize(Xdash.amazon)
test.amazon <- km(~.,  design = Xdash.amazon, response = AMAZ_MOD_FRAC)

# where is the discrapancy minimised?
plausible.test.amazon <- inputs.set(X = Xdash.amazon, y = AMAZ_MOD_FRAC,thres = 3,
                   obs = obs_amazon,
                   obs.sd = 0,
                   disc = 0,
                   disc.sd = 0.01,
                   n = 100000,
                   abt = FALSE)

## dev.new()
## #pdf(width = 7, height = 7, file = '../graphics/best_inputs_amazon.pdf')
## pairs(plausible.test.amazon$X.out, panel = dfunc.up, gap = 0, upper.panel = NULL)
## dev.off()


pred.test.amazon <- predict(test.amazon, newdata = plausible.test.amazon$X.out, type = 'UK')

dev.new()
par(mfrow = c(2,1))
hist(pred.amazon$mean, xlim = c(0,1))
hist(pred.test.amazon$mean, xlim = c(0,1))


# doesn't work if you choose the temp/precip observations - massive extrapolation!
#Xdash.standard <- c(X.standard, c(temps_obs, recursive = TRUE)[2], c(precips_obs, recursive = TRUE)[2])
Xdash.standard <- c(X.standard, mean(temps$Amazon), mean(precips$Amazon))

# Control temperature and precip and see if they give
# you anything closer to the observed Amazon.
# Do they change anything when you change them - sensitivity.

# one at a time sensitivity analysis
n=20
Xdash.oaat <- oaat.design(Xdash.amazon, n = n, hold = Xdash.standard)
colnames(Xdash.oaat) <- colnames(Xdash.amazon)

pred.oaat <- predict(test.amazon, newdata = Xdash.oaat, type = 'UK')


x11(width = 10, height = 10)

par(mfrow = c(3,3))

for(i in 1:ncol(Xdash.oaat)){
  
  ix <- seq(from=((i*n) - (n-1)), to=(i*n), by=1)
print(ix)

plot(Xdash.oaat[ix,i], pred.oaat$mean[ix], ylim = c(0,1))

}
  


stop()
## --------------------------------------------------
# gridded
library(ncdf)

# Global temperature climatology
# http://www.cru.uea.ac.uk/cru/data/temperature/

absolute.nc <- open.ncdf(paste0(datdir, 'absolute.nc'))

abs.temp <- get.var.ncdf(absolute.nc, 'tem')
abs.lat <- rev(get.var.ncdf(absolute.nc, 'lat'))
abs.lon <- get.var.ncdf(absolute.nc, 'lon')

abs.ann <- apply(abs.temp, c(1,2), mean, na.rm = TRUE)[, length(abs.lat):1 ]

image.plot(abs.lon, abs.lat, abs.ann )

world(add = TRUE)


#take the first 30 years as a baseline

cowtanandway.nc <- open.ncdf(paste0(datdir,'had4_krig_v2_0_0.nc'))

had.temps <- get.var.ncdf(cowtanandway.nc,'temperature_anomaly')
had.time <- get.var.ncdf(cowtanandway.nc,'time')
had.lat <- get.var.ncdf(cowtanandway.nc,'latitude')
had.lon <- get.var.ncdf(cowtanandway.nc,'longitude')

basechunk <- had.temps[ , , 361:720]
mean.anom <- apply(basechunk, c(1,2), mean, na.rm = TRUE)

x11()
image.plot(had.lon, had.lat,mean.anom)
world(add = TRUE)

abs.pi <- abs.ann + mean.anom

x11()
image.plot(had.lon, had.lat,abs.pi)
world(add = TRUE)


# Next, work out how to subsample. This will be easier in python!



## --------------------------------------------------

stop()
# old code from here

tempdir <- 'temp_precip_famous/'

temp.fn.list <- dir(paste0(datdir,tempdir))

# go through FULL_ID in params, and load the value to the data frame

match.list <- as.character(params$FULL_ID)

file.ix <- pmatch(match.list,temp.fn.list)

atmos.flist.ordered <- as.list(paste0(datdir,tempdir,temp.fn.list[file.ix]))

precip.ens <- load.spatial.ens(atmos.flist.ordered, 'precip_mm_srf')
temp.ens <- load.spatial.ens(atmos.flist.ordered, 'temp_mm_srf')

#nc <- open.ncdf(paste0(datdir,tempdir,temp.fn.list[[1]]))
#precip.nc <- get.var.ncdf(nc, "precip_mm_srf")
