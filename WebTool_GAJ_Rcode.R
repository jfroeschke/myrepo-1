# ABOUT ####
# This script loads Environment.csv and standardizes using the z-score method. Creates RDA ordination plot. 
# Then RDA is performed using the vegan package.
# Authors: Elizabeth Herdter
# For: Ocean Conservancy 
# Date Created: 7/21/16
# See NOTES at bottom. 

# 10/27/16
# Total mortality into Z0, Z1, Z2, Z3 and Zadult
# Remove Menhaden Effort and Landings

rm(list = ls())

#Load working directory ####
setwd("C:/Users/mdrexler/Google Drive/Ocean Conservancy_Gdrive/EL-FISH web/GAJ")

#Load pertinent packages####
library(dplyr)
library(vegan)

setwd("C:/Users/mdrexler/Google Drive/Ocean Conservancy_Gdrive/EL-FISH web/GAJ")

#Load data #### 
#and perform z-score standardization ( (x-u)/sd ) so that it has a mean of 0 and a standard dev of 1 
pred<-read.csv(file="GAJ_pred_final.csv")
resp<-read.csv(file="Env_respone_final.csv")

# Automaated Variable selection ####
#https://www.rdocumentation.org/packages/vegan/versions/2.4-2/topics/vegan-package
mod0 <- rda(resp ~1, pred) #No constraints:like a PCA # Model with intercept only
mod1 <- rda(resp ~., pred) # Model with all explanatory variables

mod_step_r2 <- ordiR2step(mod0, scope=formula(mod1)) #based on adjusted R2 and hence on eigenvalues
mod_step <- ordistep(mod0, scope=formula(mod1)) #based on P values

data.rda<-mod_step_r2

#Trying to automate the extraction of model terms and plug into a new RDA (for non manual indictor selection)
mod_step_r2$anova
variable.names(mod_step_r2$anova) 
summary(mod_step_r2$anova) 

#Plot RDA ####
#http://www.inside-r.org/packages/cran/vegan/docs/ade2vegancca
plot.new()
plot(data.rda)
plot(data.rda, type="n", xlab="", ylab="") 
ordipointlabel(data.rda, display="species", scaling="sites", add=TRUE, col="blue", cex=1.5)
#ordipointlabel(data.rda, display="sites", scaling="symm", add=TRUE)
#remove the generic plot labels and then add them at the end- see below
text(data.rda, dis="cn", cex=1.5) #dis is short for display- when dis="sp" it will plot response variables(species); when its ="si" it will plot the years(sites); when its = "cn" it will plot the predictor vectors
#points(data.rda, pch=21, col="red", bg="yellow", cex=1.2) - this is if we want the years(sites) plotted as unidentified points
#text(data.rda, "species",  col="blue", cex=0.8) #plots the response data (species)
text(data.rda, "sites", col="red", cex=1.5)
title(xlab="RDA 1 (41.78%)", ylab= "RDA 2 (8.61%)", cex.lab=1.5)  #labeled axes with %variance explained. Unfortunately it doesnt look like this is a default in the vegan package so I had to hand calculcate below. 

#Determine % variance of each axis to put on axis labels
#http://cc.oulu.fi/~jarioksa/opetus/metodi/vegantutor.pdf
# page 19-20
#total constrained variance is 26.3605. Inertia is correlations (or variance)
# So proportion of variance from first axis is = 15.065/26.3605

###############NEED TO DO ###############
#Generate stabale state grouping (circles)
#generate new axis 

