#------------------------------------------------------------------------------------------------------
# Joon-Ho Lee, School of Electrical Engineering, Korea Advanced Institute of Science and Technology
# Text book : Data-Driven Modeling & Scientific Computation(by J. Nathan Kutz) 
# p.109, Python code converted from  Matlab code in the book
#------------------------------------------------------------------------------------------------------
import numpy as np
from scipy.optimize import linprog
c = np.array([-2, -1])

A_ub = np.array([[1, 8/3], [1, 1], [2, 0], [ -1, 0], [0, -1]])
b_ub = np.array([4, 2, 3, 0, 0])

x0_bounds = (0, None)
x1_bounds = (0, 5.0)

bounds = [x0_bounds, x1_bounds]

result = linprog(c, A_ub=A_ub, b_ub=b_ub, bounds=bounds)

print(result)