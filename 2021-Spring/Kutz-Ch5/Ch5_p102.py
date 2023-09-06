#------------------------------------------------------------------------------------------------------
# Joon-Ho Lee, School of Electrical Engineering, Korea Advanced Institute of Science and Technology
# Text book : Data-Driven Modeling & Scientific Computation(by J. Nathan Kutz) 
# p.102, Python code converted from  Matlab code in the book
#------------------------------------------------------------------------------------------------------
x=[[] for _ in range(100)]
y=[[] for _ in range(100)]
f=[[] for _ in range(100)]

x[1]=3; y[1]=2 # initial guess
f[1]=x[1]**2+3*y[1]**2 # initial function value
for j in range(1,101):
    tau=(x[j]**2 +9*y[j]**2)/(2*x[j]**2 + 54*y[j]**2)
    x[j+1]=(1-2*tau)*x[j] # update values
    y[j+1]=(1-6*tau)*y[j]
    f[j+1]=x[j+1]**2+3*y[j+1]**2    
    print('Number of iteration = {0:2d}, x= {1:7.4f}, y= {2:7.4f}, diff= {2:9.6f}'.format(j, x[j+1], y[j+1], abs(f[j+1]-f[j])))
    if abs(f[j+1]-f[j])<10**(-6) : break # check convergence

print(' ')
print('Number for conservation= {0}'.format(j))
print('Conserved vlaues x= {0:0.4f}, y= {1:0.4f}'.format(x[j+1], y[j+1]) )   