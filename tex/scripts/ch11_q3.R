# functions for n1(t+1) and n2(t*1)
f1 = function(r, N1, N2) {r*N1*(1-N2)}
f2 = function(p, N1, N2) {N1 + p*N2}

# set r and p
r = 2 # 1.5
p = 0.5

# get equilibrium abundances
N1star = (1-1/r)*(1 - p)
N2star = (1 - 1/r)

# make empty vectors for storing results
n1 = rep(NA, 1000)
n2 = rep(NA, 1000)

# set starting abundances
n1[1] = 0.1 #N1star
n2[1] = 0 #N2star

# simulate for 1000 time steps
for(i in 1:999) {
  n1[i+1] = f1(r, n1[i], n2[i])
  n2[i+1] = f2(p, n1[i], n2[i])
}

# plot output

matplot(1:1000, cbind(n1,n2), type = "l",
        xlab="time", ylab="abundance")
legend("topright", c("n1", "n2"), col=1:2, lty=1:2, bty = "n")





