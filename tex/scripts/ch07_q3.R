runf<-function(r,N0=1) {
r=r;					               # growth rate
K=1000;                      # carrying capacity
s=-r/K;                      # set s to hold K constant
dt=1;                        # set time step size
timevec=seq(0, 20, by=dt);   # vector of time steps
Nvec=numeric(length(timevec)); # empty vector for storing abundances
N=N0;                        # initial abundance
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

pdf("images/ch07_q3.pdf", width=6, height=5)
  par(mfrow=c(2,2), mar=c(4,4,1,1))
  r=3
  K=1000
  s=-r/K
  out<-runf(3,1)
  timevec=out[,1]
  Nvec=out[,2]
  
  ## Example 1: N vs. t
  plot(timevec, Nvec, type="l")    # plot N vs. t
  
  ## Example 2: dNNdt vs. N
  #note diff(x) function calculates x[i+1]-x[i] for all i
  dN = diff(Nvec);                 #get dN
  dt = diff(timevec);              # get dt
  Nt = Nvec[1:(length(Nvec)-1)];   #N(t)
  # note - we need to remove the last element, since
  # there is no corresponding future observation from
  # which to calculate dNNdt
  dNNdt = 1/Nt*dN/dt;              # calculate per-capita growth rate
  plot(Nt, dNNdt);
  abline(h=0, lty=2);              # add line at dNNdt = 0
  abline(a=r, b=s);                # add line following dNNdt = r+sN
  
  ## Example 3: dNdt vs. N
  dNdt = dN/dt;                    # calculate population growth rate
  plot(Nt, dNdt);
  abline(h=0, lty=2);              # add line at dNNdt = 0
  #dN/Ndt = r+sN
  #dN/dt = (r+sN)*N
  Nseq<-seq(0, 1400, by=1)         # a sequence of N values for plotting
  dNdt_est<-(r+s*Nseq)*Nseq        # analytical estimate of dNdt
  lines(Nseq, dNdt_est)            # add line
  
  ## Example 4: N(t) vs. N(t-1)
  Nt = Nvec[1:(length(Nvec)-1)];    #N(t)
  N_tp1 = Nvec[2:length(Nvec)];     #N(t+1)
  plot(Nt, N_tp1)
  lines(Nseq, Nseq+(r+s*Nseq)*Nseq)
dev.off()



