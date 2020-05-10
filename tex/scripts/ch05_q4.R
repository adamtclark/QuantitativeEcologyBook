r=1; s=-0.001;   # set parameter values
N0 = 1;          # initial abundance
times = seq(0, 20, by=0.1) # a vector of times

# function for calculating analytical solution
# define variables that are used in the function
logisticfun=function(r, s, N0, t) {
  # operations to perform within the function 
  N = 1/((s/r+1/N0)*exp(-r*times)-s/r)
  # value to return at the end of the function
  return(N)                                
}

Nt = logisticfun(r=r, s=s, N0=N0, t=times) # run function
plot(times, Nt, type="l")                  # plot results


pdf("images/ch05_q4.pdf", width=5, height=4)
par(mar=c(4,4,1,1))
plot(times, Nt, type="l")                  # plot results
dev.off()