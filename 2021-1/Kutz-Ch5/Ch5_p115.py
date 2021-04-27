#------------------------------------------------------------------------------------------------------
# Joon-Ho Lee, School of Electrical Engineering, Korea Advanced Institute of Science and Technology
# Text book : Data-Driven Modeling & Scientific Computation(by J. Nathan Kutz) 
# p.115, Python code converted from  Matlab code in the book
#------------------------------------------------------------------------------------------------------
from matplotlib import pyplot as plt
import numpy as np
from scipy.optimize import fmin
import math

def func(c,x): return c[0] * np.cos(c[1]*x) + c[2]

m = 200 # number of generations
n = 50 # number of trials
n2 = 10 # number of trials to be kept

A = 12 + np.random.randn(n)
B = math.pi/12 + np.random.randn(n)
C = 60 + np.random.randn(n)

E   = [0 for _ in range(n )]

E_best   = [0 for _ in range(m )]

Ak1 = [0 for _ in range(n2 )]
Bk1 = [0 for _ in range(n2)]
Ck1 = [0 for _ in range(n2 )]

data_x = np.linspace(1, 24,24)
data_y = [75,77,76,73,69,68,63,59,57,55,54,52,50,50,49,49,49,50,54,56,59,63,67,72]
I1 = np.ones(24)

cc=0
for jgen in range(1, m + 1):
    for j in range(0, n ): # evaluate objective function            
      
        E[j] = np.sum( (A[j] * np.cos(B[j]*data_x) + C[j] *I1- data_y)**2) 
    
    ix = sorted(range(len(E)), key=lambda k: E[k])
    E_best[jgen-1] = E[ix[0]]
   
    for k in range(0,n2):
        Ak1[k] = A[ix[k]] # best 10 solutions
        Bk1[k] = B[ix[k]]
        Ck1[k] = C[ix[k]]
     
    Ak2 = Ak1 + np.random.randn(n2)/jgen # 10 new mutations    
    Bk2 = Bk1 + np.random.randn(n2)/jgen
    Ck2 = Ck1 + np.random.randn(n2)/jgen

    Ak3 = Ak1 + np.random.randn(n2)/jgen # 10 new mutations
    Bk3 = Bk1 + np.random.randn(n2)/jgen
    Ck3 = Ck1 + np.random.randn(n2)/jgen

    Ak4 = Ak1 + np.random.randn(n2)/jgen # 10 new mutations
    Bk4 = Bk1 + np.random.randn(n2)/jgen
    Ck4 = Ck1 + np.random.randn(n2)/jgen

    Ak5 = Ak1 + np.random.randn(n2)/jgen # 10 new mutations
    Bk5 = Bk1 + np.random.randn(n2)/jgen
    Ck5 = Ck1 + np.random.randn(n2)/jgen
          
    #A = np.concatenate((Ak1,Ak2),axis=1) # group new 50
    A = np.concatenate([Ak1, Ak2, Ak3, Ak4, Ak5])    
    B = np.concatenate([Bk1, Bk2, Bk3, Bk4, Bk5])
    C = np.concatenate([Ck1, Ck2, Ck3, Ck4, Ck5])
  

plt.plot(data_x, data_y,'bo')

data_x2 = np.linspace(1, 24, 100)

p = [A[0],B[0],C[0]]

print(p)

plt.plot(data_x2, func(p,data_x2),'r')

plt.legend(["data", "p.115"])

plt.xlabel('x')
plt.ylabel('y')

plt.show()

data_x_E_best = np.linspace(1, m, m)

plt.plot(data_x_E_best, E_best,'r')

plt.xlabel('Iteration number')
plt.ylabel('E')

plt.show()
