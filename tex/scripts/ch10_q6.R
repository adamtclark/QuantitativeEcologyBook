# Set up parameters
r1= 1.2; s12=-0.4; s11=-1.2;     # Parameters for species 1.
r2= 1.4; s21=-0.7; s22=-0.8;     # Parameters for species 2.

# Compute useful sub-formulae.
p = r1*s22 -r2*s12;
q = r2*s11 -r1*s21; 
a = s12*s21 -s11*s22;
b = r1*s22*(s21-s11)+r2*s11*(s12-s22);
c = -p*q;

# Compute the equilibria.
x00=0; y00=0; # (at the origin)
x10=-r1/s11; y10=0; # (on the x-axis)
x01=0; y01=-r2/s22; # (on the y-axis)
x11=p/a; y11=q/a; # (at the interior)

# Compute the corresponding
# four pairs of eigenvalues
# (real part only).
v00= r1; w00=r2;     # (at the origin)
v10=-r1; w10=q/s11;  # (at the x-axis)
v01=-r2; w01=p/s22;  # (at the y-axis)
# (at the interior)
v11=(-b-sqrt(pmax(0, b^2-4*a*c)))/(2*a);
w11=(-b+sqrt(pmax(0, b^2-4*a*c)))/(2*a);

v00; w00
v10; w10
v01; w01
v11; w11


