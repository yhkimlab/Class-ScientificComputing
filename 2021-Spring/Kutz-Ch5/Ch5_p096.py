#------------------------------------------------------------------------------------------------------
# Joon-Ho Lee, School of Electrical Engineering, Korea Advanced Institute of Science and Technology
# Text book : Data-Driven Modeling & Scientific Computation(by J. Nathan Kutz) 
# p.96, Python code converted from  Matlab code in the book
#------------------------------------------------------------------------------------------------------
import math


def f(x):
    return x**4+10*x*math.sin(x**2)


a=-2; b=1 # initial interval
c=(-1+math.sqrt(5))/2 # golden section

x1 = c*a + (1-c)*b
x2 = (1-c)*a + c*b

f1 = f(x1)
f2 = f(x2)

for j in range(1,101):
    if f1<f2 :#% move right boundary
        b=x2; x2=x1; f2=f1
        x1=c*a+(1-c)*b
        f1=f(x1)
    else :#% move left boundary
        a=x1; x1=x2; f1=f2
        x2=(1-c)*a + c*b
        f2=f(x2)
    
    print('Number of iteration = {0:2d}, a= {1:0.4f}, b= {2:0.4f}, c= {3:0.4f}'.format(j,a,b,c))
    if (b-a)<10**(-6) : break # break if close

print(' ')
print('Number for conservation= {0}'.format(j))
print('Conserved vlaues a= {0:0.4f}, b= {1:0.4f}, c= {2:0.4f}'.format(a,b,c))