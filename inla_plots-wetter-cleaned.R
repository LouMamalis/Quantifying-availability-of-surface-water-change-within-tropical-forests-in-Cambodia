##This script creates the plots for the INLA model results
#it creates a rug plot to show the logit probability of extreme flooding with distance to roads
#and it also creates a forest plot to show the logit probability of extreme flooding and presence of different landuses 

#use this to install INLA 
install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)

#load packages
library(raster)
library(rgeos)
library(fields)
library(rgdal)
library(maptools)

#load the model results 
load("R:/rsrch/cb751/phd/lcm566/Giant Ibis projeto/Changes to surface water/Inlamodelling/inla-modelling/outputs/inla_model_results_wetter.Rdata")

##load data stack
load('covariate_stack_bino.Rdata')

##here i need to run a small part of the inla model
#convert to a SpatialPointsDataFrame - as()
#this is like a normal dataframe but has spatial points within it
cov_spdf <- as(cov_stack, "SpatialPointsDataFrame")
cov_spdf <- cov_spdf[!is.na(rowSums(cov_spdf@data)), ]
names(cov_spdf) <- c("sw_transition", "initial_sw", "elevation", "PAs", "ELCs", "roads") 


#Transform and scale covariates:
#distributions of covariate data - good to scale. mean of 0 and sd of -1
cov_spdf$lroads <- log(cov_spdf$roads + 1)  # log(x+1) for right skewed data containing zeros
cov_spdf$lroads_s <- c(scale(cov_spdf$lroads)) 
cov_spdf$elev_s <- c(scale(cov_spdf$elevation))

#reduce the dataset to only the ones that were rarely or never flooded (can get wetter) selecting for 3's and 4's...
#those that are less than 3, irreg flooded or perm flooded (1s and 2s)
#converted into a true or false statement 
cov_spdf_wetter <- cov_spdf[cov_spdf$initial_sw >2,]

#finds rows where in column initial_sw 1 and 2 are the wettest
#adding in a new column to the spdf dataframe 
#selects those squares that have been wetting over time - within the column sw_transition. will give 0's and 1's where its got much wetter
cov_spdf_wetter$sig_wet <- (cov_spdf_wetter$sw_transition >= 2) * 1 #* 1 converts a logical true or false then converted to 1 or 0 so that INLA can understand it 

##make a mesh
#define independently of spatial observations - dont need every pairwise combination
#estimates of spatial random effects to know whats going on 
max.edge = 0.05  # ~6km at equator. Max. triangle length. resolution 
mesh <- inla.mesh.2d(
  loc=coordinates(cov_spdf),
  offset = c(0.05, 0.5),    # Expanding outside actual locations. Determine inner and outer offset (buffer creation) 
  #set areas for fine and larger mesh areas, useful for patchy datasets 
  max.edge=c(max.edge, max.edge*2),  # For filling in area
  # discretization accuracy
  cutoff=max.edge/2)

#and the spde: applying the mesh to the locations, linking it --> a structure
A = inla.spde.make.A(mesh=mesh, loc=data.matrix(coordinates(cov_spdf_wetter)))  # Combine actual points with mesh

#covariates:
#as a matrix, every location of coords passed to the mesh
Xcov = data.frame(intercept=1, 
                  lroads_s = cov_spdf_wetter$lroads_s,
                  elev_s = cov_spdf_wetter$elev_s,
                  ELCs = cov_spdf_wetter$ELCs, 
                  PAs = cov_spdf_wetter$PAs)
Xcov = as.matrix(Xcov)
colnames(Xcov)
stck <- inla.stack(tag='est',     # stack is INLA data structure
                   data=list(transitions=cov_spdf_wetter$sig_wet),
                   effects=list(
                     # - The Model Components
                     s=1:mesh$n,
                     # - The "s" is means "spatial"
                     Xcov=Xcov), #covariates set up above
                   # - The second is all fixed effects
                   A = list(A, 1)
                   # - First projector matrix is for 's'
                   # - second is for 'fixed effects'
)

##starting to create the plot 
for (i in 1:length(res$marginals.fixed)) {
  tmp = inla.tmarginal(function(x) x, res$marginals.fixed[[i]]) 
  plot(tmp, type = "l", xlab = paste("Fixed effect marginal", i, ":", colnames(Xcov)[i]), ylab = "Density")
  abline(v = 0, lty = 2)
}

unscale <- function(x, scale.params = sc.p) {
  return((x * scale.params$'scaled:scale') + scale.params$'scaled:center')
}

#function to add letter to top left corner
put.fig.letter <- function(label, x=NULL, y=NULL, 
                           offset=c(0, 0), ...) {
  coords <- c(0.09,0.9)
  this.x <- grconvertX(coords[1] + offset[1], from="nfc", to="user")
  this.y <- grconvertY(coords[2] + offset[2], from="nfc", to="user")
  text(labels=label[1], x=this.x, y=this.y, xpd=T, cex = 1.5, ...)
}

##Rug plot 
#plot for looking at distance to roads and logit probability of land flooding
#x data defined here
sc.p <- attributes(scale(cov_spdf_wetter$lroads))
xs <- exp(unscale(seq(min(cov_spdf_wetter$lroads_s), max(cov_spdf_wetter$lroads_s), len = 100)))

#create the plot here
plot(xs, res$summary.lincomb.derived$"0.5quant"[grep("roads", rownames(res$summary.lincomb.derived))], type = "n", axes = F, bty = "l", 
     ylim = range(c(res$summary.lincomb.derived[grep("roads", rownames(res$summary.lincomb.derived)), "0.025quant"],
                    rev(res$summary.lincomb.derived[grep("roads", rownames(res$summary.lincomb.derived)), "0.975quant"]))),
     xlim = c(1000, max(cov_spdf_wetter$roads)),  
     xlab = "Distance to road (km)",
     ylab = "Probability of extreme flooding", pch = 20, col = rgb(0,0,0,0.05),
     log = "x")

#creates the box for the plot
box(bty = "o")

#creates a polygon which shows the upper and lower quantiles, although it has now stopped working weirdly... 
polygon(c(xs, rev(xs)), (c(res$summary.lincomb.derived[grep("roads", rownames(res$summary.lincomb.derived)), "0.025quant"],
                           rev(res$summary.lincomb.derived[grep("roads", rownames(res$summary.lincomb.derived)), "0.975quant"]))),
        border = NA,   col = '#ea9999')
lines(xs, (res$summary.lincomb.derived$`0.5quant`[grep("roads", rownames(res$summary.lincomb.derived))]), lwd = 2, col = rgb(0.7, 0, 0.1))
axis(2)

#controls the values and scale of the x axis
axis(1, at = c(1, 5001, 10001, 20001, 30001), labels = c(0, 5, 10, 20, 30)) #at in meters and labels in km, put smaller values back 

##add a rug plot of raw data  points
#100 lines that represent the density of the points in the raw data, ones that didn't get wetter - failures
rug(quantile(cov_spdf_wetter$roads[cov_spdf_wetter$sig_wet == 0], seq(0, 1, 0.01)))

#those points that did get wetter - successes 
rug(quantile(cov_spdf_wetter$roads[cov_spdf_wetter$sig_wet == 1], seq(0, 1, 0.01)), side = 3)

#this adds a letter in the corner of the plot for ref
put.fig.letter(label = 'B)', font = 2)

##making forest plot to explore the influence of ELCs and PAs
#creates a forest plot to look at the probability of drying against the presence of protected areas and economic land concessions
#1:3 selects the scale for the x axis, includes the 3 different land categories 
#4:6 selects the columns of data that we cant to use - 0.25quant, 0.5quant and 0.975quant 
#landuse 1 = intercept (if PA and ELCs are both 0), landuse 2 = ELCs, landuse 3 = PAs
plot(c(1,3), c(range(res$summary.lincomb.derived[grep("landuse", rownames(res$summary.lincomb.derived)), 4:6])), 
     type = 'n',
     xaxt = "n",
     xlab = "Land Use",
     ylab = "Probability of extreme flooding")
axis(1, at = 1:3, labels = c("Other","Econmic Land Concessions", "Protected Areas"))
for(i in 1:3){
  lines(rep(i, 2), res$summary.lincomb.derived[grep("landuse", rownames(res$summary.lincomb.derived)), c(4, 6)][i,],)
  points(i, res$summary.lincomb.derived[grep("landuse", rownames(res$summary.lincomb.derived)), 5][i],
         pch = 19) #defines the point size/shape
}

#adds a letter to the corner for labeling the plot 
put.fig.letter(label = 'D)', font = 2)