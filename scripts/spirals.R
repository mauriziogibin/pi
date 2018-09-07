g#-------------------------------------------------------------------------------------------
#
# Script to prepare data for Zonal Attach
#
# By: Maurizio Gibin
# Code by: Maurizio Gibin
# Contact: maurizio.gibin@jrc.ec.europa.eu
#
# DISCLAIMER: THE CODE HAS BEEN TESTED ON LINUX MACHINES
# IF YOU ENCOUNTER ANY PROBLEM PLEASE GET IN CONTACT.
#--------------------------------------------------------------------------------------------

library(data.table)

# Housekeeping
rm(list = ls())
options(scipen = 999)
# options(digits=4)

#- Clear workspace
# rm(list=ls())
#- Settings paths
cDIR  <- '~/work/pi/'        #LINUX
# cDIR   <- 'C:/'    #WINDOWS

codePath   <- paste0(cDIR,"scripts/")         
dataF      <- paste0(cDIR,"data/")           
outPath    <- paste0(cDIR,"results/")   


golden.ratio = (sqrt(5) + 1)/2
fibonacci.angle=360/(golden.ratio^2)
c=1
num_points=2000
x=rep(0,num_points)
y=rep(0,num_points)

for (n in 1:num_points) {
  r=c*sqrt(n)
  theta=fibonacci.angle*(n)
  x[n]=r*cos(theta)
  y[n]=r*sin(theta)
}
plot(x,y,axes=FALSE,ann=FALSE,pch=19,cex=1,asp=1,col=sapply(s$figure,colByfigure))
