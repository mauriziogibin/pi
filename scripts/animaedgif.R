y_vals<-rnorm(1000) 
x_vals<-1:1000 
plot(y_vals, col=(257+ii*10)) 
for(ii in 1:5) 
{ 
  y_vals<-rnorm(1000) 
  points(x_vals, y_vals, col=(257+ii*10)) 
} 


plot(1:10, 1:4)
text(0:25, 0.6, 0:25, cex = 0.5)
points(0:25, 0:25, pch = 0:25, bg = "grey")
require(stats) # for rnorm
plot(0:10000, 0:10000, ylim=c(0,4),type = "n")  # setting up coord. system
points(rnorm(200), rnorm(200), col = "red")
points(rnorm(100)/2, rnorm(100)/2, col = "blue", cex = 1.5)

#set working directory
setwd('~/work/pi/images/tmp/')

frames = 50

for(i in 1:frames){
  # creating a name for each plot file with leading zeros
  if (i < 10) {name = paste('000',i,'plot.png',sep='')}
  
  if (i < 100 && i >= 10) {name = paste('00',i,'plot.png', sep='')}
  if (i >= 100) {name = paste('0', i,'plot.png', sep='')}
  x = seq(0, i, 1)
  f.3 = dbinom(x, size = i, prob=.3)
  f.7 = dbinom(x, size = i, prob=.7)
  
  #saves the plot as a .png file in the working directory
  png(name)
  plot(x, f.3, type='h',
       xlim = c(0,frames), ylim = c(0,.7), ylab ='probability',
       main = paste('Binomial density with n =', i), col = 'red')
  lines(x,f.7,type='h',col='blue')
  # text(45, .6, 'p = .3', col='red')
  # text(45, .6, 'p = .7', col='blue', pos=1)
  dev.off()
}
# The important part of the code is the png function. This function saves the plot as a .png file in the working directory (there are aslo jpeg, bmp, and tiff functions that work the same way). Anything between the png and dev.off() will be saved in the plot.
# 
# After running this code there will be 50 .png files in your working directory. Now it’s time to use ImageMagick. From a command line navigate to the directory where the .png files are saved and enter the following command.
# 
# 
# $ convert *.png -delay 3 -loop 0 binom.gif