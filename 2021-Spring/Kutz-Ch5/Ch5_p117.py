#------------------------------------------------------------------------------------------------------
# Joon-Ho Lee, School of Electrical Engineering, Korea Advanced Institute of Science and Technology
# Text book : Data-Driven Modeling & Scientific Computation(by J. Nathan Kutz) 
# p.117, Python code converted from  Matlab code in the book
#------------------------------------------------------------------------------------------------------
import numpy as np
import math
from geneticalgorithm import geneticalgorithm as ga

def f(X): 
    data_x = np.linspace(1, 24,24)
    data_y = [75,77,76,73,69,68,63,59,57,55,54,52,50,50,49,49,49,50,54,56,59,63,67,72]
    I1 = np.ones(24)
        
    return np.sum( (X[0] * np.cos(X[1]*data_x) + X[2] *I1- data_y)**2)

varbound = np.array([[10,15], [math.pi/20,math.pi/4],[50,70]])

model = ga(function=f,dimension=3,variable_type='real',variable_boundaries=varbound)


model.run()

convergence = model.report
solution    = model.output_dict




print(solution)
print(solution['variable'])
print(solution['variable'][0])
print(solution['variable'][1])
print(solution['variable'][2])
