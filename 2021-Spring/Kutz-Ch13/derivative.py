import numpy as np


def spectral_derivative(f, x):
    
    # define target function
    u = f(x)
    
    # set parameter
    n = len(x)
    L = int(-2*x[0])
    
    # FFT method
    ut  = np.fft.fft(u)  # FFT the target function

    # 2pi scaled k domain
    k = (2*np.pi) * np.fft.fftfreq(n,d=L/n).astype(np.complex64)

    # derivative using FFT (spectral derivative)
    ut1 = (1j*k) * ut

    # inverse transform
    u1 = np.real(np.fft.ifft(ut1))

    return u1


def second_order_finite_difference(f, x):

    # define target function
    u = f(x)
    n = len(x) # number of grid points
    dx = x[1] - x[0] # grid spacing
    ux = np.zeros(n)


    # forward method for initial region
    #ux[0] = (-3*u[0]+4*u[1]-u[2]) / (2*dx)
    
    # second-order ceter method
    for i in range(1,n-1):
        ux[i] = (u[i+1]-u[i-1]) / (2*dx)
        
    # backward method of final region
    #ux[n-1] = (3*u[n-1]-4*u[n-2]+u[n-3]) / (2*dx)
    
    return ux


def fourth_order_finite_difference(f, x):

    # define target function
    u = f(x)
    n = len(x) # number of grid points
    dx = x[1] - x[0] # grid spacing
    ux = np.zeros(n)

    # forward method for initial region
    #ux[0] = (-3*u[0]+4*u[1]-u[2]) / (2*dx)
    #ux[1] = (-3*u[1]+4*u[2]-u[3]) / (2*dx)
    
    # fourth-order ceter method
    for j in range(2,n-2):
        ux[j] = (-u[j+2]+8*u[j+1]-8*u[j-1]+u[j-2]) / (12*dx)

    # backward method of final region
    #ux[n-2] = (3*u[n-2]-4*u[n-3]+u[n-4]) / (2*dx)
    #ux[n-1] = (3*u[n-1]-4*u[n-2]+u[n-3]) / (2*dx)
    
    return ux

