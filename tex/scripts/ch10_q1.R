# Set up parameters
r1= 1.2; s12=-0.4; s11=-1.2;     # Parameters for species 1.
r2= 1.4; s21=-0.7; s22=-0.8;     # Parameters for species 2.

K1 = -r1/s11; # Carrying capacity 1
K2 = -r2/s22; # Carrying capacity 2

# Set up plot
xmax=2.5; ymax=2.5; grid=25;  # Size and number of grid cells.
dxy=xmax/grid;                # Spacing between grid cells.
xy0=dxy/2;                    # Indentation at left and top.
a=0.1;                        # Length of arrows.
x=y=xy0;                      # Generate points uniformly across

#pdf("images/ch10_q1.pdf", width=6, height=6)
par(mar=c(4,4,2,2))
# Make empty plot
plot(c(0, xmax), c(0, ymax),
     xlab = "", ylab = "", type="n")
abline(h=0, lwd=2, col="red")   # Add red line for x-axis
abline(v=0, lwd=2, col="blue")  # Add blue line for y-axis
# Add colored axis labels
mtext("N1", side = 1, col="blue", line=2.6)
mtext("N2", side = 2, col="red", line=2.6)

# Add lines for dN1/dt = 0 and dN2/dt = 0
# a is y-intercept, b is slope
# N1 = -r1/s11 - s12*N2
# x = -r1/s11 - s12*y
# - y*s12 = -r1/s11 - x
# y = -(-r1/s11 - x)/s12
# y = r1/s11/s12 + x*(1/s12)
abline(a = r1/s11/s12, b = (1/s12), col="blue") # species 2
# N2 = r2 + s21*N1
# y = r2 + s21*x
abline(a = (r2/-s22), b = s21, col="red") # species 2

# Add equilibria
points(0, 0, cex=1.5)
points(0, -r2/s22, cex=1.5)
points(-r1/s11, 0, cex=1.5)

# N2 = -r2/s22 + s21*N1
# N1 = -r1/s11 - s12*N2
# - s12*N2 = -r1/s11 - N1
# N2 = - (-r1/s11 - N1)/s12
# N2 = r1/s11/s12 + N1*(1/s12)

# -r2/s22 + s21*N1 = r1/s11/s12 + N1*(1/s12)
# s21*N1 - N1*(1/s12) = r1/s11/s12 + r2/s22
# N1*(s21 - 1/s12) = r1/s11/s12 + r2/s22
# N1 = (r1/s11/s12 + r2/s22)/(s21 - 1/s12)
# N1 = (r1*s22 + r2*s11*s12)/((s11*s12*s21 - s11)*s22)

# N2 = -r2/s22 + s21*N1
# N2 = -r2/s22 + s21*((r1/s11/s12 + r2/s22)/(s21 - 1/s12))
# N2 = (r1*s21*s22 + r2*s11)/((s11*s12*s21 - s11)*s22)

points((r1*s22 + r2*s11*s12)/((s11*s12*s21 - s11)*s22),
       (r1*s21*s22 + r2*s11)/((s11*s12*s21 - s11)*s22), pch=16, cex=1.5)

# Run loop until the "break" command is reached
while(1) {                    # the phase space with small random
  N1=x+runif(1,-.03,.03);     # variations in their locations, for
  N2=y+runif(1,-.03,.03);     # best appearance in the graph.
  d1=(r1+s11*N1+s12*N2)*N1;   # Compute the magnitude of change in
  d2=(r2+s21*N1+s22*N2)*N2;   # each population from each point.
  b=sqrt(d1^2+d2^2);          # Display a vector of fixed length.
  arrows(N1, N2,              # Starting point for arrows
         N1+d1*a/b, N2+d2*a/b,# Ending point for arrows
         length = 0.05,       # Arrow point length
         col=adjustcolor("black", alpha.f = 0.5)); # Make slightly arrows transparent
  
  x=x+dxy;                    # Advance horizontally along the row.
  if(x>xmax)                  # At the end of the row, move to the
  { x=xy0; y=y+dxy; }         # beginning of the next row.
  if(y>ymax) {break;}         # Stop the loop when completed.
}
#dev.off()
