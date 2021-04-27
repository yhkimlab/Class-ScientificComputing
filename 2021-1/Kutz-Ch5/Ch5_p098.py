#------------------------------------------------------------------------------------------------------
# Joon-Ho Lee, School of Electrical Engineering, Korea Advanced Institute of Science and Technology
# Text book : Data-Driven Modeling & Scientific Computation(by J. Nathan Kutz) 
# p.98, Python code converted from  Matlab code in the book
#------------------------------------------------------------------------------------------------------
import math


def f(x):
    return x**4+10*x*math.sin(x**2)

x1=-1.5; x2=-1; x3=-.5 # initial guesses
f1=f(x1)
f2=f(x2)
f3=f(x3)

for j in range(1,101):
    x0 =(x1+x2)/2 - ( (f2-f1)*(x3-x1)*(x3-x2) )/( 2*( (x2-x1)*(f3-f2)-(f2-f1)*(x3-x2) ) )
    if x0>x2:
        x1=x2; f1=f2
        x2=x0; f2=f(x0)
    else:
        x3=x2; f3=f2
        x2=x0; f2=f(x0)
    
    print('Number of iteration = {0:2d}, x1= {1:0.6f}, x2= {2:0.6f}, x3= {3:0.6f}'.format(j,x1,x2,x3))
    if abs(x2-x3)<10**(-6) or abs(x2-x1)<10**(-6) : break

print(' ')
print('Number for conservation= {0}'.format(j))
print('Conserved vlaues x1= {0:0.4f}, x2= {1:0.4f}, x3= {2:0.4f}'.format(x1,x2,x3))