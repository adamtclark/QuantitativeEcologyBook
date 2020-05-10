runf<-function(X) {
r=1; s=-0.001;               # set parameter values
dt=1/X;                      # set time step size
timevec=seq(0, 20, by=dt);   # vector of time steps
Nvec=numeric(length(timevec)); # empty vector for storing abundances
N=1;                         # initial abundance
t=0;                         # set initial time to zero
i=1;                         # counter for tracking position in N vector

while(i<=length(timevec)) {
  dN=(r+s*N)*N*dt;           # calculate dN
  N=N+dN;                    # add dN to the previous N value
  if(N<0) N=0;               # prevent negative populations
  Nvec[i]=N;                 # store new N value
  t=t+dt;                    # step forward in time
  i=i+1;                     # add one to counter
}
return(cbind(timevec, Nvec))
}

pdf("images/ch05_q1.pdf", width=5, height=4)
par(mar=c(4,4,1,1))
out<-runf(1)
plot(out[,1], out[,2], type="l", xlab="timevec", ylab="Nvec", lwd=1.5) # plot results
out<-runf(2)
lines(out[,1], out[,2], col="red", lwd=1.5)
out<-runf(5)
lines(out[,1], out[,2], col="blue", lwd=1.5)
out<-runf(50)
lines(out[,1], out[,2], col="goldenrod", lwd=1.5)
legend("bottomright", c("X = 1", "X = 2", "X = 5", "X = 50"), lty=1, col=c(1, "red", "blue", "goldenrod"), bty="n", lwd=1.5)
dev.off()



