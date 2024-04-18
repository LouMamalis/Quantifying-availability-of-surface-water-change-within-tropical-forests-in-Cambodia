#this is the script for the actual inla model running 
#the outputs of this script are the inla model results for the areas getting drier 
#this is a binomial model

###Read in the required packages###
library(raster)
library(rgeos)
library(fields)
library(rgdal)
library(maptools)

install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
library(INLA)
library(plotrix) #this enables the use of axis.break 

###Load data stack###
load('outputs/covariate_stack_bino.Rdata')

#view
plot(cov_stack)

###Convert to a SpatialPointsDataFrame###
#convert the covariate stack to a SpatialPointsDataFrame (this is like a normal dataframe but has spatial points within it)
cov_spdf <- as(cov_stack, "SpatialPointsDataFrame")
cov_spdf <- cov_spdf[!is.na(rowSums(cov_spdf@data)), ]
names(cov_spdf) <- c("sw_transition", "initial_sw", "elevation", "PAs", "ELCs", "roads") #picks out the covariates included within this

#transform and scale covariates:
#distributions of covariate data - good to scale. mean of 0 and sd of -1
cov_spdf$lroads <- log(cov_spdf$roads + 1)  #do this:log(x+1) for right skewed data containing zeros
cov_spdf$lroads_s <- c(scale(cov_spdf$lroads)) 
cov_spdf$elev_s <- c(scale(cov_spdf$elevation))

###Select those getting drier over time###
#reduce dataset to those that are getting drier from the initial_sw state
#these pixels that are less than 3 which denote the surface water categories - irreg flooded (1) or perm flooded (2)
#then converted into a true or false statement 
cov_spdf_drier <- cov_spdf[cov_spdf$initial_sw <3,]

#selects those pixels that have been drying over time
#creates a new column sig_dry which gives a 0 or 1 then a T or F 
cov_spdf_drier$sig_dry <- (cov_spdf_drier$sw_transition <= -2) * 1 

####Make a mesh for the INLA model####
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
# plot(mesh)

###Apply the mesh to the spde created above###
#applying the mesh to the locations, linking it --> a structure (A)
#Combine actual points with mesh
A = inla.spde.make.A(mesh=mesh, loc=data.matrix(coordinates(cov_spdf_drier)))  

###Linking it to the covariates###
#as a matrix, every location of coords passed to the mesh
#creates a dataframe
Xcov = data.frame(intercept=1, 
                  lroads_s = cov_spdf_drier$lroads_s,
                  elev_s = cov_spdf_drier$elev_s,
                  ELCs = cov_spdf_drier$ELCs, 
                  PAs = cov_spdf_drier$PAs)

#makes the df into a matrix
Xcov = as.matrix(Xcov)
colnames(Xcov)

#then make a stack, that is the INLA data structure
stck <- inla.stack(tag='est',     
                   data=list(transitions=cov_spdf_drier$sig_dry),
                   effects=list(#lists the Model Components
                     s=1:mesh$n,#s = "spatial"
                     Xcov=Xcov), #covariates set up above
                   #The second is all fixed effects
                   A = list(A, 1) #have put A back instead of cov_spdf_drier 
                   #First projector matrix is for 's'
                   #second is for 'fixed effects'
)

#setting up the priors 
spde = inla.spde2.pcmatern(mesh, 
                           prior.range = c(0.1, 0.5),  #guess (10km) and confidence (how often above and below guess).
                           prior.sigma = c(1, 0.1), #once we have reached the max range how large is the variation at this point
                           constr = TRUE)

###Setting up the elements to feed into the inla model###
#model, transitions given the matrix of covariates and the spatial effects 
#writing up the linear predictor of the model in a formula object
formula = transitions ~ -1 + Xcov + f(s, model=spde)

#setting the likelihood family (gaussian/poisson/binomial)
family = 'binomial' #regression family
control.family = list(hyper = list(prec = list(
  prior = "pc.prec", fixed = FALSE)))

#make some linear combinations, for effect plots:
#explore these models, want to know the effects of the covariates
#estimate things as it fits the model 
roads_lc <- inla.make.lincombs(Xcov1 = rep(1, 100),
                               Xcov2 = seq(min(cov_spdf$lroads_s), max(cov_spdf$lroads_s), len = 100))
names(roads_lc) <- paste0("roads", 1:100)
elev_lc <- inla.make.lincombs(Xcov1 = rep(1, 100),
                              Xcov3 = seq(min(cov_spdf$elev_s), max(cov_spdf$elev_s), len = 100))
names(elev_lc) <- paste0("elevation", 1:100)
landuse_lc <- inla.make.lincombs(Xcov1 = rep(1, 3), #intercept, nothing special
                                 Xcov4 = c(0,1,0), #ELCs
                                 Xcov5 = c(0,0,1)) #PAs
names(landuse_lc) <- paste0("landuse", 1:3)

#combining all the covariates together
all_lc <- c(elev_lc, roads_lc, landuse_lc)

###Fitting the model###
#construct a stack with the data and the projection matrix:
res <- inla(formula, #specifies the linear predictor
            data=inla.stack.data(stck), #data frame that contains the data
            control.predictor=list(A = inla.stack.A(stck), #list with the specification of several predictor variables
            compute=T), # compute=T to get posterior for fitted values
            family = family, #family variable set above, 
            lincomb = all_lc, #creating info for creating plots of the effects - hash out the lines above that create the lincombs. e.g. might want to plot elevation against transition 
           # control.family = control.family, #dont want this part for the binomial model
            #control.compute = list(config=T, dic=T, cpo=T, waic=T), 
            # - Model comparisons
            control.inla = list(int.strategy='eb'),
            # - faster computation
            #control.inla = list(int.strategy='grid'),
            # - More accurate integration over hyper-parameters
            verbose=FALSE)

#save the results output 
save(res, file = 'inla_model_results_drier.Rdata')

###load the model results###
#can just load this without running the bit above 
load("R:/rsrch/cb751/phd/lcm566/Giant Ibis projeto/Changes to surface water/Inlamodelling/inla-modelling/results/inla_model_results_drier.Rdata")

###looking at the details of the inla model itself###
names(res)

#looking at the fixed effects of the model 
res$summary.fixed

#looking at the hyper parameters of the model
res$summary.hyper
res$marginals.fixed$Xcov4
res$summary.lincomb.derived
res$summary.lincomb
res$summary.linear.predictor

#looking at some plots of the covariates
plot(values(cov_stack[[1]]), values(cov_stack[[2]]))
boxplot(values(cov_stack[[2]])~values(cov_stack[[1]]))

#created a really simple linear model
#this did not include the spatial component 
mod1 <- lm(sw_transition~lroads_s + elev_s + ELCs + PAs, data = cov_spdf@data)

#have a look at the results 
summary(mod1)

#plot looking at the changing in dryness against distance to roads 
plot(jitter(values(cov_stack[[1]]), 0.8)~values(cov_stack[[5]]), col = rgb(0,0,0,0.05))
plot(cov_stack)

###PLOTTING###
#this section makes the plots from the inla results for the more extreme drying events
for (i in 1:length(res$marginals.fixed)) {
  tmp = inla.tmarginal(function(x) x, res$marginals.fixed[[i]]) 
  plot(tmp, type = "l", xlab = paste("Fixed effect marginal", i, ":", colnames(Xcov)[i]), ylab = "Density")
  abline(v = 0, lty = 2)
}

unscale <- function(x, scale.params = sc.p) {
  return((x * scale.params$'scaled:scale') + scale.params$'scaled:center')
}

#add letter to top left corner of each of the plots for reference 
put.fig.letter <- function(label, x=NULL, y=NULL, 
                           offset=c(0, 0), ...) {
  coords <- c(0.09,0.9)
  this.x <- grconvertX(coords[1] + offset[1], from="nfc", to="user")
  this.y <- grconvertY(coords[2] + offset[2], from="nfc", to="user")
  text(labels=label[1], x=this.x, y=this.y, xpd=T, cex = 1.5, ...)
}

#plot for looking at distance to roads and logit probability of land drying
#plot type n = nothing so can set up coords correctly 
#using the unscale function 
sc.p <- attributes(scale(cov_spdf_drier$lroads))
xs <- exp(unscale(seq(min(cov_spdf_drier$lroads_s), max(cov_spdf_drier$lroads_s), len = 100)))

#create the plot here
plot(xs, res$summary.lincomb.derived$"0.5quant"[grep("roads", rownames(res$summary.lincomb.derived))], type = "n", axes = F, bty = "l", 
     ylim = range(c(res$summary.lincomb.derived[grep("roads", rownames(res$summary.lincomb.derived)), "0.025quant"],
                    rev(res$summary.lincomb.derived[grep("roads", rownames(res$summary.lincomb.derived)), "0.975quant"]))),
     xlim = c(400, max(cov_spdf_drier$roads)),  
     xlab = "Distance to road (km)",
     ylab = "Logit prob of extreme increase of drying", pch = 20, col = rgb(0,0,0,0.05),
     log = "x")

#creates the box for the plot 
box(bty = "o")

#adds the shaded in polygon which represents the 0.025 quant and the 0.975 quant
polygon(x=c(xs, rev(xs)), y=(c(res$summary.lincomb.derived[grep("roads", rownames(res$summary.lincomb.derived)), "0.025quant"],
                               rev(res$summary.lincomb.derived[grep("roads", rownames(res$summary.lincomb.derived)), "0.975quant"]))),
        border = NA,   col = '#ea9999') 

#plots the mean (0.5 quant) values 
lines(xs, (res$summary.lincomb.derived$`0.5quant`[grep("roads", rownames(res$summary.lincomb.derived))]), lwd = 2, col = rgb(0.7, 0, 0.1))
axis(2)

#controls the values and scale of the x axis 
axis(1, at = c(1, 5001, 10001, 15001, 20001, 30001), labels = c(0, 5, 10, 15, 20, 30)) #at in meters and labels in km, put smaller values back 

###Adding a rug plot values###
#100 lines that represent the density of the points in the raw data
#those that didnt get drier plotted along the bottom
rug(quantile(cov_spdf_drier$roads[cov_spdf_drier$sig_dry == 0], seq(0, 1, 0.01)))

#adds those points that did get drier and are plotted along the top
#these are pixels that are closer to the roads - closer to roads, more likely to be drier
rug(quantile(cov_spdf_drier$roads[cov_spdf_drier$sig_dry == 1], seq(0, 1, 0.01)), side = 3)

#this adds a letter in the corner of the plot for ref
put.fig.letter(label = 'A)', font = 2)

###Forest plot to explore the influence of ELCs and PAs###
#look at the probability of drying against the presence of protected areas and economic land concessions
#1:3 selects the scale for the x axis, includes the 3 different land categories 
#4:6 selects the columns of data that we cant to use - 0.25quant, 0.5quant and 0.975quant 
#landuse 1 = intercept (if PA and ELCs are both 0), landuse 2 = ELCs, landuse 3 = PAs
plot(c(1,3), c(range(res$summary.lincomb.derived[grep("landuse", rownames(res$summary.lincomb.derived)), 4:6])), 
     type = 'n',
     xaxt = "n",
     xlab = "Land Use",
     ylab = "Logit prob of extreme increase of drying")
axis(1, at = 1:3, labels = c("Other","ELCs", "PAs"))
for(i in 1:3){
  lines(rep(i, 2), res$summary.lincomb.derived[grep("landuse", rownames(res$summary.lincomb.derived)), c(4, 6)][i,],)
  #col =  rgb(0.7, 0, 0.1)) this makes it red, dont want it red 
  points(i, res$summary.lincomb.derived[grep("landuse", rownames(res$summary.lincomb.derived)), 5][i],
         pch = 19) #defines the point size/shape
  #col =  rgb(0.7, 0, 0.1), pch = 20)
}

#adds a letter to the corner for labeling the plot 
put.fig.letter(label = 'C)', font = 2) 
