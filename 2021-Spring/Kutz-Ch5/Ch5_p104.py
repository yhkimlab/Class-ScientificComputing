#------------------------------------------------------------------------------------------------------
# Joon-Ho Lee, School of Electrical Engineering, Korea Advanced Institute of Science and Technology
# Text book : Data-Driven Modeling & Scientific Computation(by J. Nathan Kutz) 
# p.104, Python code converted from  Matlab code in the book
#------------------------------------------------------------------------------------------------------
from matplotlib import pyplot as plt
import numpy as np
from scipy.optimize import fmin

def func(c,x): return c[0] * np.cos(c[1]*x) + c[2]

def err_func(p, x, y): return np.sqrt(( (func(p,x)-y)**2 ).sum() /24.0)

n = 24

xdata = np.linspace(1, 24,24)
ydata = [75,77,76,73,69,68,63,59,57,55,54,52,50,50,49,49,49,50,54,56,59,63,67,72]

p0 = [50.0, 0.1, 50.0] # initial parameter value
p = fmin(err_func, p0, args=(xdata,ydata), xtol=1e-6)

print ('estimater parameters: ', p)

plt.plot(xdata, ydata,'bo')

xdata2 = np.linspace(1, 24, 100)

plt.plot(xdata2, func(p,xdata2),'g')

plt.legend(["data", "p.104"])

plt.xlabel('x')
plt.ylabel('y')

plt.show()