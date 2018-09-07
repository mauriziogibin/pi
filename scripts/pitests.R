#-------------------------------------------------------------------------------------------
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
# library(openxlsx)

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

setwd(dataF)

options(digits = 22) # 22 is max allowed
pi_calc <- 3
x <- 2
n <- 630
piDT   <- NULL
temp   <- NULL
for(i in 1:n) {
  y <- x + 1
  z <- x + 2
  combine_with_pi_calc <- 4 / (x * y * z)
  test <- i %% 2 == 0 
  if (isTRUE(test)) {
    pi_calc <- pi_calc - combine_with_pi_calc
  }
  else {
    pi_calc <- pi_calc + combine_with_pi_calc
  }
  x <- z
  print(pi_calc)
  temp    <- data.table('n'=i,'pi'=pi_calc)
  piDT    <- rbind(piDT,temp)
  # points(piDT[i,n],piDT[i,pi],col='red')
}
plot(0:1000,0:1000,col='red',xlab='Number of iteractions', ylab='Value of pi', ylim=c(3.1412,3.1419),type = "n")
points(piDT[,n],piDT[,pi],col='blue',cex=0.3)
lines(piDT[,n],piDT[,pi],col='red',cex=0.3)

for(i in 1:n) {
  points(piDT[i,n],piDT[i,pi],col='yellow')  
}


colByfigure   <- function(x){
  if ( x==1) {
    return('yellow')
  } else if (x==2) {
    return('blue')
  } else if (x==3) {
    return('red')
  } else if (x==4) {
    return('purple')
  } else if (x==5) {
    return('orange')
  } else if (x==6) {
    return('green')
  } else if (x==7) {
    return('brown')
  } else if (x==8) {
    return('black')
  } else {
    return('white')
  }
}
pie <- read.csv("http://oeis.org/A000796/b000796.txt", header=FALSE, sep=" ")
dig <- 10000 # up to 20000 digits 
pistring <- paste(c(pie[1,]$V2, ".", head(pie[-1,], dig-1)$V2), collapse="")
t   <- strsplit(pistring,split = '')
s           <- as.data.frame(t)
s   <- as.data.frame(s[-c(2),])
names(s)    <- 'figure' 
s$figure   <- as.numeric(as.character(s$figure))
sapply(s$figure,colByfigure)

golden.ratio = (sqrt(5) + 1)/2
fibonacci.angle=360/(golden.ratio^2)
c=1
num_points=dig
x=rep(0,num_points)
y=rep(0,num_points)

for (n in 1:num_points) {
  r=c*sqrt(n)
  theta=fibonacci.angle*(n)
  x[n]=r*cos(theta)
  y[n]=r*sin(theta)
}
plot(
  x,
  y,
  axes = FALSE,
  ann = FALSE,
  pch = 16,
  cex = 0.6,
  asp = 1,
  col = sapply(s$figure, colByfigure),
  ylim=c(-100,100),
  xlim=c(-100,100)
)
# lines(x,y,col=sapply(s$figure,colByfigure),cex=0.1)

lenRowX   <- 100

rep(1:10, 10)
s$x   <- rep(1:lenRowX,nrow(s)/lenRowX)
plot(
  s$x,
  s$y,
  axes = FALSE,
  ann = FALSE,
  pch = 16,
  cex = 0.6,
  asp = 1,
  col = sapply(s$figure, colByfigure),
  ylim=c(0,10),
  xlim=c(0,100)
)
