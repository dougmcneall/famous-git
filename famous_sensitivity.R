# famous_sensitivity.R

# ------------------------------------------------------------------------
# 0. 
# 
# ------------------------------------------------------------------------

source('famous_common.R')
library(sensitivity)


# ------------------------------------------------------------------------
# 2. One-at-a-time sensitivity analysis using an emulator. Output
# is forest fraction.
# ------------------------------------------------------------------------

# Build an emulator
out.list <- ls(pattern = "MOD_FRAC")

fit.amazon <- km(~.,design = X, response = AMAZ_MOD_FRAC)
fit.seasia <- km(~.,design = X, response = SEASIA_MOD_FRAC)
fit.congo <- km(~.,design = X, response = CONGO_MOD_FRAC)
fit.namerica <- km(~.,design = X, response = NAMERICA_MOD_FRAC)
fit.global <- km(~.,design = X, response = GLOB_MOD_FRAC)

n <- 21

X.oat <- oaat.design(X, n = n, hold = X.standard)
colnames(X.oat) <- colnames(X)

## build a matrix of OAT predictions
oat.mean.mat <- matrix(nrow = nrow(X.oat), ncol = length(out.list))
oat.sd.mat <- matrix(nrow = nrow(X.oat), ncol = length(out.list))

for(i in 1:length(out.list)){
  
  dat <- get(out.list[i])
  
  fit <- km(~.,design = X, response = get(out.list[i]))
  pred <- predict(fit, newdata = X.oat, type = 'UK')
  oat.mean.mat[, i ] <- pred$mean
  oat.sd.mat[, i ] <- pred$sd
}

colnames(oat.mean.mat) <- out.list
colnames(oat.sd.mat) <- out.list


col.amaz <- acc[1]
col.namerica <- acc[2]
col.seasia <- acc[3]
col.congo <- acc[4]
col.global <- acc[5]

pal.list.order <- c(1,4,5,2,3)
pal.chosen <- acc[pal.list.order]

## Plot predicted AMAZON fraction output vs each input 
#dev.new(width = 8, height = 8)
pdf(width = 8, height = 8, file = '../graphics/paper/amaz_oat_sens.pdf')
par(mfrow = c(2,4), las = 1, mar = c(5,0.5,2,0.5), oma = c(0,5,0,0), fg = 'grey')
for(i in 1: ncol(X.norm)){

  ix <- seq(from = ((i*n) - (n-1)), to =  (i*n), by = 1)

  plot(X[, i], AMAZ_MOD_FRAC, xlab = colnames(X)[i],
       col = NULL, pty = 'n', ylim = c(0,1), bty = 'n',
       ylab = '', cex.lab = 1.5, cex.axis = 1.5, axes = FALSE)
  axis(1, cex.lab = 1.5)
  
  if (i==1){axis (2, cex.axis = 1.5)
             mtext(side = 2, line = 3.5, text = 'FOREST FRACTION', las = 0, col = 'black')
           }

  if (i==5){axis (2, cex.axis = 1.5)
          mtext(side = 2, line = 3.5, text = 'FOREST FRACTION', las = 0, col = 'black')
          }
    
  for(j in 1:length(out.list)){
    
    # plot the mean point for the sensitivity analysis
    points(X.oat[ix, i], oat.mean.mat[ix, j],
           pch = j, col = (acc[pal.list.order])[j], type = 'l', cex = 1.2, lwd = 3)

    col.chosen <- (acc[pal.list.order])[j]
    col.transp <- adjustcolor(col.chosen, alpha = 0.5)
    
    polygon(x = c(X.oat[ix, i], rev(X.oat[ix, i])),
            y =c((oat.mean.mat[ix, j] - oat.sd.mat[ix, j]), rev((oat.mean.mat[ix, j] + oat.sd.mat[ix, j]))),
            col = col.transp, border = col.transp
            )
  }
}

plot(1:10, type = 'n', axes = FALSE, xlab = '', ylab = '')
legend('top', legend = c('Amazon', 'Congo', 'Global', 'N America', 'SE Asia'), col = acc[pal.list.order], lty = 'solid', lwd = 1, pch = NA, bty = 'n', text.col = 'black', fill = adjustcolor(pal.chosen, alpha = 0.5), cex = 1.5, border = NA) 

dev.off()


# ------------------------------------------------------------------------------
# 2a Sensitivity analysis using the extended FAST algorithm of Saltelli
# et al, using the R package "sensitivity"
# ------------------------------------------------------------------------------

fit.amazon.norm <- km(~.,design = X.norm, response = AMAZ_MOD_FRAC)
fit.seasia.norm <- km(~.,design = X.norm, response = SEASIA_MOD_FRAC)
fit.congo.norm <- km(~.,design = X.norm, response = CONGO_MOD_FRAC)
fit.namerica.norm <- km(~.,design = X.norm, response = NAMERICA_MOD_FRAC)
fit.global.norm <- km(~.,design = X.norm, response = GLOB_MOD_FRAC)

# generate the design to run the emulator at, using fast99
x <- fast99(model = NULL, factors = colnames(X), n = 1000,
q = "qunif", q.arg = list(min = 0, max = 1))

# run the emulator at the sensitivity analysis design points
fast.pred.amaz <- predict(fit.amazon.norm, newdata = x$X, type = 'UK')
fast.pred.seasia <- predict(fit.seasia.norm, newdata = x$X, type = 'UK')
fast.pred.congo <- predict(fit.congo.norm, newdata = x$X, type = 'UK')
fast.pred.namerica <- predict(fit.namerica.norm, newdata = x$X, type = 'UK')
fast.pred.global <- predict(fit.global.norm, newdata = x$X, type = 'UK')

# Calculate the sensitivity indices
fast.amaz <- tell(x, fast.pred.amaz$mean)
fast.congo <- tell(x, fast.pred.congo$mean)
fast.seasia <- tell(x, fast.pred.seasia$mean)
fast.namerica <- tell(x, fast.pred.namerica$mean)
fast.global <- tell(x, fast.pred.global$mean)

bp.convert <- function(fastmodel){
  # get the FAST summary into an easier format for barplot
  fast.summ <- print(fastmodel)
  fast.diff <- fast.summ[ ,2] - fast.summ[ ,1]
  fast.bp <- t(cbind(fast.summ[ ,1], fast.diff))
  fast.bp
}

# Plot the sensitivity indices
#x11(width = 5.8, height = 10)

pdf(width = 6.5, height = 7, file = '../graphics/paper/FAST_histograms.pdf')
par(mfrow = c(5,1), mar = c(0,3,0,2), las = 1, oma = c(5,4,1,2), fg = 'darkgrey', xaxs = 'i', cex.axis = 1.1)
barplot(bp.convert(fast.amaz), ylim = c(0,1), col = c(col.amaz, 'lightgrey'),axisnames = FALSE)
mtext(side = 3, adj = 0.05, line = -3, text = 'Amazon', cex = 1.2, col = 'black')
legend('top', legend = c('Main effect', 'Interaction'),
       fill = c(col.amaz, 'lightgrey'), bty = 'n', text.col = 'black', cex = 1.3)

barplot(bp.convert(fast.seasia), ylim = c(0,1), col = c(col.seasia, 'lightgrey'), axisnames = FALSE,
        axes = FALSE)
axis(2, at = c(0,0.2,0.4,0.6,0.8,1), labels = c(0,0.2,0.4,0.6,0.8,NA))
mtext(side = 3, adj = 0.05, line = -3, text = 'SE Asia', cex = 1.2, col = 'black')

barplot(bp.convert(fast.congo), ylim = c(0,1), col = c(col.congo, 'lightgrey'), axisnames = FALSE, axes = FALSE)
axis(2, at = c(0,0.2,0.4,0.6,0.8,1), labels = c(0,0.2,0.4,0.6,0.8,NA))
mtext(side = 3, adj = 0.05, line = -3, text = 'Congo', cex = 1.2, col = 'black')

barplot(bp.convert(fast.namerica), ylim = c(0,1), col = c(col.namerica, 'lightgrey'), axisnames = FALSE, axes = FALSE)
axis(2, at = c(0,0.2,0.4,0.6,0.8,1), labels = c(0,0.2,0.4,0.6,0.8,NA))
mtext(side = 3, adj = 0.05, line = -3, text = 'N America', cex = 1.2, col = 'black')

barplot(bp.convert(fast.global), ylim = c(0,1), col = c(col.global, 'lightgrey'))
mtext(side = 3, adj = 0.05, line = -3, text = 'Global', cex = 1.2, col = 'black')

mtext(side = 2, text = 'Sensitivity Index', outer = TRUE, las = 0, line = 2, cex = 1.3, col = 'black')

dev.off()
# ------------------------------------------------------------------

fast.total.order <- function(fastmodel){
  fast.summ <- print(fastmodel)
  total.order <- fast.summ[,2]
  total.order

}


total.effects <- rbind(fast.total.order(fast.amaz),
              fast.total.order(fast.congo),
              fast.total.order(fast.seasia),
              fast.total.order(fast.namerica)
              )

total.effect.all.forests <- apply(total.effects,2,sum)



# -------------------------------------------------------------------------------
# 5. Two-at-a-time measure of implausibility
# (Here a function derived from the commented code above)
# -------------------------------------------------------------------------------

pairs.taat.imp <- function(X, y, X.target, obs, obs.sd = 0, disc = 0, disc.sd = 0, n = 21, title.text = '',
                           pdf.out = FALSE, filename){

  taat <- taat.design(X, n = n, means = X.target)
  colnames(taat$des) <- colnames(X)
  des <- data.frame(taat$des)

  fit <- km(~., design = X, response = y)

  taat.pred.stan <- predict(fit, newdata = X.target, type = 'UK')
  taat.pred <- predict(fit, newdata = des, type = 'UK')

  taat.impl <- impl(em = taat.pred$mean, em.sd = taat.pred$sd,
                    disc = disc, obs = obs, disc.sd = disc.sd, obs.sd = obs.sd)
  
  if(pdf.out == TRUE){
    pdf(width = 7, height = 7, file = filename)
  }
  
  else{
    dev.new(width = 7, height = 7)
  }
  
  par(mar = c(0.5,0.5,0.5,0.5), oma = c(2,2,5,0.1), mgp = c(2,1,0), font.lab = 2, cex.lab = 1.5)
  nf <- layout(matrix(c(1,22,0,0,0,0,
                        2,7,0,0,0,0,
                        3,8,12,0,0,0,
                        4,9,13,16,0,0,
                        5,10,14,17,19,0,
                        6,11,15,18,20,21
                        ), 6,6, byrow = TRUE))

  npc <- n*n
  
  for(i in 1:21){
  
    i.ix <- ((i * npc) - (npc - 1)) : (i* npc)
    x.ix <- taat$ix[1,i]
    y.ix <- taat$ix[2,i]

    cplotShort(taat$des[i.ix, x.ix],
               taat$des[i.ix, y.ix],
               z =  taat.impl[i.ix],
               col = byr,
               pch = 20,
               xlab = colnames(X)[x.ix],
               ylab = colnames(X)[y.ix],
               axes = FALSE
               )
    points(X.target[x.ix], X.target[y.ix], col = 'black', bg = 'green', cex = 2, pch = 21)

    if(i %in% c(1,2,3,4,5,6)) mtext(colnames(X)[y.ix], side = 2, line = 0.5, cex = 1)
    if(i %in% c(6,11,15,18,20,21)) mtext(colnames(X)[x.ix], side = 1, line = 1, cex = 1)
    if(i == 1) mtext(title.text, side = 3, line = 1, cex = 1.2)

  }
                                        #zr <- range(taat.impl)
  zr <- c(0,3)
  par(mar = c(1,1,1,6))
  plot(1:10, type = 'n', axes = FALSE, xlab = '', ylab = '')

  

  image.plot(legend.only = TRUE,
             zlim = zr,
             col = byr,
             legend.args = list(text = 'Implausibility', side = 3, line = 1),
             legend.width = 2,
             #legend.shrink = 1.5,
             horizontal = FALSE
             )
  par(mar = c(0,0,0,0))
  plot(1:10, type = 'n', axes = FALSE, xlab = '', ylab = '')
  legend('topleft',legend = 'Default\nParameter', pch = 21, pt.bg = 'green',cex = 1.3, pt.cex = 1.5,
         bty = 'n')
  
    if(pdf.out == TRUE){
     dev.off()
    }

}

pairs.taat.imp(X = X.norm,
               y = AMAZ_MOD_FRAC,
               X.target = X.stan.norm,
               obs = obs_amazon,
               obs.sd = 0.1,
               disc = 0,
               disc.sd = 0,
               n = 21,
               title.text = 'AMAZON',
               pdf.out = TRUE,
               filename = '../graphics/paper/taat_amaz.pdf'
               )

pairs.taat.imp(X = X.norm,
               y = CONGO_MOD_FRAC,
               X.target = X.stan.norm,
               obs = obs_congo,
               obs.sd = 0.1,
               disc = 0,
               disc.sd = 0,
               n = 21,
               title.text = 'CONGO',
               pdf.out = TRUE,
               filename = '../graphics/paper/taat_congo.pdf'
               )

pairs.taat.imp(X = X.norm,
               y = NAMERICA_MOD_FRAC,
               X.target = X.stan.norm,
               obs = obs_namerica,
               obs.sd = 0.1,
               disc = 0,
               disc.sd = 0,
               n = 21,
               title.text = 'NAMERICA',
               pdf.out = TRUE,
               filename = '../graphics/taat_namerica.pdf'
               )

pairs.taat.imp(X = X.norm,
               y = SEASIA_MOD_FRAC,
               X.target = X.stan.norm,
               obs = obs_seasia,
               obs.sd = 0.1,
               disc = 0,
               disc.sd = 0,
               n = 21,
               title.text = 'SEASIA',
               pdf.out = TRUE,
               filename = '../graphics/taat_seasia.pdf'
               )

pairs.taat.imp(X = X.norm,
               y = GLOB_MOD_FRAC,
               X.target = X.stan.norm,
               obs = obs_glob,
               obs.sd = 0.1,
               disc = 0,
               disc.sd = 0,
               n = 21,
               title.text = 'GLOBAL',
               pdf.out = TRUE,
               filename = '../graphics/taat_global.pdf'
               )


