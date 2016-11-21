README

Data for “The impact of structural error on parameter constraint in a climate model” by Doug McNeall, Jonny Williams, Ben Booth, Richard Betts, Peter Challenor, Andy Wiltshire, and David Sexton. Published in Earth System Dynamics, 2016
contact dougmcneall@gmail.com

The R data file “famous_forest_fraction.RData” contains a number of R objects:

full_frac         Data frame containing 100 ensemble members of FAMOUS. Unique model run ID, input parameters and aggregated forest fraction
bl.frac.ens       Global broadleaf tree grid box forest fraction, rows match the rows of full_frac, columns are lats x longs of the model grid (by lats)
nl.frac.ens       Global needleleaf tree grid box forest fraction, rows match the rows of full_frac, columns are lats x longs of the model grid (by lats)
lats              Latitudes for FAMOUS model grid
longs             Longitudes for FAMOUS model grid
obs               Aggregated “Observations” of forest fraction
X.standard        Standard or default parameters for FAMOUS land surface


Useful functions:

remap.famous = function(dat,longs,lats, shift = FALSE){
  # reshape a map in vector form so that fields() package function image.plot() 
  #  (for example) will plot it correctly
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

