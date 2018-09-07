# For this exercise we assume to draw random points inside the square on the [-1,1] unit, 
# and thus the value of Pi = 4(# random pts insid cirlce / # random pts in square)


# Montecarlo method, based on throwing a dart on a dartboard,
# The number of darts in side th dartboard dividided by the number of darts thrown
# *4 gives PI
sim.pi <- function(iterations = 1000) {
   # Generate two vectors for random points in unit circle
   x.pos <- runif(iterations, min=-1, max=1)
   y.pos <- runif(iterations, min=-1, max=1)
   # Test if draws are inside the unit circle
   draw.pos <- ifelse(x.pos^2 + y.pos^2 <= 1, TRUE, FALSE)
   draws.in <- length(which(draw.pos == TRUE))
   # Estimate Pi
   return(4*(draws.in/iterations))
}

pi   <- sim.pi(100)
plot(pi)

pie=function(n){
   count=0
   for(i in 1:n){
      if(runif(1,-1,1)^2+runif(1,-1,1)^2<=1)
         count=count+1
   }
   return(4*(count/n))
}
pi2   <- pie(100000000)

for(i in 1:1000){
   if(i%%2 == 1) {
      pi.plus = 4 / (i * (i + 1) * (i + 2))
      pi.plus=pi.plus+pi.plus
   } else{
      pi.minus = 4 / (i * (i + 1) * (i + 2))
      pi.minus=pi.minus+pi.minus
   }
   print(3+pi.plus-pi.minus)
   }

for(i in 1:10){
   if(i%%2==1){
      minus   <- (1/i)
      t   <- i+2
      plus    <- (1/(t))
    } else {next}
    print(c(i,t))
   # pi=4*(1-minus+plus)
}
sum   <- 0
for(i in 1:100){
   x   <- i
   ((-1^ x) * (1 / (1 + 2*x)))
}
