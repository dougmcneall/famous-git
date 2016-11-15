
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

## plausible.namerica <- inputs.set(X = X.norm, y = NAMERICA_MOD_FRAC,thres = 3,
##                    obs = obs_namerica,
##                    obs.sd = 0,
##                    disc = 0,
##                    disc.sd = 0.01,
##                    n = 100000,
##                    abt = FALSE)

## plausible.seasia <- inputs.set(X = X.norm, y = SEASIA_MOD_FRAC,thres = 3,
##                    obs = obs_seasia,
##                    obs.sd = 0,
##                    disc = 0,
##                    disc.sd = 0.01,
##                    n = 100000,
##                    abt = FALSE)

## plausible.global <- inputs.set(X = X.norm, y = GLOB_MOD_FRAC,thres = 3,
##                    obs = obs_glob,
##                    obs.sd = 0,
##                    disc = 0,
##                    disc.sd = 0.01,
##                    n = 100000,
##                    abt = FALSE)


# -----------------------------------------------------------------
# 4. Create maps of the forest fraction at the "best" values of the
# Amazon, the congo, and the difference between them.
#
# Emulate every point in the ensemble individually
# -----------------------------------------------------------------

create.emu.obj.list <- function(X, Y){
  # each list element should be an emu.obj

  d <- ncol(Y)
  emu.obj.list <- vector(mode = 'list', length = d)

  for(i in 1:d){

    emu.obj <- NULL
    emu.obj$X <- X
    emu.obj$y <- Y[, i]
    
    emu.obj.list[[i]] <- emu.obj
    
  }
  
  emu.obj.list

}

km.wrapper <- function(emu.obj){

  # emu.obj should have two components; X and y 
    fit <- km(design = emu.obj$X, response = emu.obj$y, control = list(trace = FALSE))

    fit 
}

km.pred.wrapper <- function(emu.pred.obj){

  # split the data
  emu.obj.list <- create.emu.obj.list(emu.pred.obj$X.train, emu.pred.obj$Y.train)

  # emulate the list
  direct.em <- lapply(emu.obj.list, FUN = km.wrapper)
  # do the predictions
  direct.pred.list <- lapply(direct.em,
                             FUN = predict,
                             newdata = matrix(emu.pred.obj$X.target, nrow = 1),
                             type = 'UK')

  direct.pred.vec <- c(lapply(direct.pred.list, FUN = function(x){x$mean}), recursive = TRUE)

  #return(list(pred = direct.pred.vec, km.list  = direct.em))
  direct.pred.vec
  # save the list of km objects as well
  
}


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


match.list <- as.character(params$FULL_ID)

file.ix <- pmatch(match.list,dir(famousdir))

# lev = 1 is broadleaf forest (2 would be needleleaf)
bl.frac.ens <- load.frac.ens(fn.list = fn.list[file.ix], var ='fracPFTs_mm_srf', lev = 1)

na.map <- is.na(bl.frac.ens)

bl.frac.ens.nona <- bl.frac.ens

na.map.onelayer <- is.na(bl.frac.ens.nona[1, ])

bl.frac.ens.nona[na.map] <- 0

# scale up to make everything a bit more stable
bl.frac.scaled <- bl.frac.ens.nona * 100


direct.em.loop <- function(X, Y, newdata){

  d <- ncol(Y)
  print(d)
  n.em <- nrow(newdata)

  Y.em <- matrix(nrow = n.em, ncol = d)

  for(i in 1:d){

    print(i)

    y <- c(Y[ ,i], recursive = TRUE)

    # try emulating, see if it works, if not leave the NAs
        fit <- try(km(~., design = X, response = y, control = list(trace = FALSE, pop.size = 50)), silent = TRUE)
    
        if(class(fit) == "try-error")
          {
            Y.em[, i] <- rep(NA, n.em)
            rm(fit)
          }
          
        else
          {
            pred <- predict(fit, newdata = newdata, type = 'UK')
            Y.em[, i] <- pred$mean
          }
    
  }

  Y.em
  
}

## standard.norm.em <- direct.em.loop(X.norm, bl.frac.scaled, newdata = matrix(X.stan.norm, nrow = 1))

## dev.new(width = 7, height = 5)
## image.plot(longs, rev(lats), remap.famous(standard.norm.em, longs, lats,  shift = 190), col = byr[4:11],
##            zlim = c(0,1), axes = FALSE, xlab = '', ylab = '', main = 'standard')


amazon.em <- direct.em.loop(X.norm, bl.frac.scaled, newdata = plausible.amazon$X.out)
congo.em <- direct.em.loop(X.norm, bl.frac.scaled, newdata = plausible.congo$X.out)

# Find the MEAN of the ensemble generated at best, or plausible parameters 
amazon.mean <- apply((amazon.em / 100), 2, mean)
congo.mean <- apply((congo.em / 100), 2, mean)

#Map the MEAN of the ensemble generated at best, or plausible parameters

save.image(file = '../data/best_maps.Rdata')

pdf(width = 5, height = 9, file = '../graphics/paper/best_X_maps.pdf')
#dev.new(width = 5, height = 9)
par(mfrow = c(3,1), mar = c(1,2,2,2))

image.plot(longs, rev(lats), remap.famous(amazon.mean, longs, lats,  shift = 190), col = byr[4:11],
           zlim = c(0,1), axes = FALSE, xlab = '', ylab = '', main = 'Amazon parameters')

image.plot(longs, rev(lats), remap.famous(congo.mean, longs, lats,  shift = 190), col = byr[4:11],
           zlim = c(0,1), axes = FALSE, xlab = '', ylab = '', main = 'Africa parameters')

image.plot(longs, rev(lats), remap.famous(amazon.mean - congo.mean, longs, lats,  shift = 190), col = byr,
           zlim = c(-.5,.5), axes = FALSE, xlab = '', ylab = '', main = 'Amazon - Africa ')
dev.off()

stop()



# emulate the set of inputs that are NROY when you give the amazon a model discrepancy term.
plausible.em <- direct.em.loop(X.norm, bl.frac.scaled, newdata = plausible.disc.amazon$X.out)
plausible.mean <- apply((plausible.em / 100), 2, mean)


pdf(width = 7, height = 5, file = '../graphics/Amazon_disc_map.pdf')
#dev.new(width = 7, height = 6)
image.plot(longs, rev(lats), remap.famous(plausible.mean, longs, lats,  shift = 190), col = byr[4:11],
           zlim = c(0,1), axes = FALSE, xlab = '', ylab = '', main = 'With Amazon discrepancy')
dev.off()


## # find where there are no NAs
## list.ix <- which(na.map.onelayer == FALSE)


## # some of these are NA, some are zero
## bl.frac.list <- create.emu.obj.list(X.norm, bl.frac.ens)

## test.list <- mclapply(bl.frac.list[490:501], FUN = km.wrapper)

## direct.pred.list.amazon <- mclapply(test.list, FUN = predict, newdata = plausible.amazon$X.out, type = 'UK')
## direct.pred.list.congo <- mclapply(test.list, FUN = predict, newdata = plausible.congo$X.out, type = 'UK')
## direct.pred.list.standard <- mclapply(test.list, FUN = predict, newdata = X.stan.norm, type = 'UK')

## get.ens.em <- function(em.list, list.ix){

##   n.em <- length(em.list[[1]]$mean)
##   n.cols <- length(em.list)

##   out.mat <- matrix(ncol = n.cols, nrow = n.em)

  
##   for(i in 1:length(list.ix)){

##     out.mat[, i] <- em.list[[i]]$mean

##   }

##   out.mat
  
## }

## amazon.frac.em <- get.ens.em(direct.pred.list.amazon)
## congo.frac.em <- get.ens.em(direct.pred.list.congo)
## standard.frac.em <- get.ens.em(direct.pred.list.standard)

## amazon.best <- plausible.amazon$X.out
## congo.best <- plausible.congo$X.out


## save(amazon.frac.em,congo.frac.em,standard.frac.em,amazon.best, congo.best,
##      file = '../data/frac_em.Rdata')

