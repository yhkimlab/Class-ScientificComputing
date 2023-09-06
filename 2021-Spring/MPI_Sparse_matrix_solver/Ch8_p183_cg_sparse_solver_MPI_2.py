#------------------------------------------------------------------------------------------------------
# Joon-Ho Lee, School of Electrical Engineering, Korea Advanced Institute of Science and Technology
# Text book : Data-Driven Modeling & Scientific Computation(by J. Nathan Kutz) 
# This is a serial version for solving Ax=B in p.183
#------------------------------------------------------------------------------------------------------
from mpi4py import MPI
import numpy as np
import datetime
from scipy.optimize import fmin
from scipy.sparse import csr_matrix
def test_f(x,a,b):
    return (x-2.5+a[0]+b)**2

def MPI_if(N_proc, proc_i, N_data):
        
    n_local = int(N_data/N_proc) 
    n_residue = int(N_data - n_local*N_proc )
    
    if proc_i < n_residue : 
        n_local = n_local + 1
    
        ii = proc_i * n_local + 1
        fi = ii  + n_local - 1
    else :
        ii = proc_i*n_local + 1 + n_residue
        fi = ii  + n_local - 1

    return ii, fi 



#---- main ----------------------------

comm = MPI.COMM_WORLD
N_proc = comm.Get_size()
proc_i = comm.Get_rank()

start_time = datetime.datetime.now() 


comm.barrier()

tau = 0.2
Tol = 1e-14
'''
mA = np.array([
[-4,  1,  0,  1,  1,  0,  0,  0,  0,  0,  0,  0,  1,  0,  0,  0], 
[ 1, -4,  1,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  1,  0,  0], 
[ 0,  1, -4,  1,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  1,  0], 
[ 1,  0,  1, -4,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  1],
[ 1,  0,  0,  0, -4,  1,  0,  1,  1,  0,  0,  0,  0,  0,  0,  0], 
[ 0,  1,  0,  0,  1, -4,  1,  0,  0,  1,  0,  0,  0,  0,  0,  0], 
[ 0,  0,  1,  0,  0,  1, -4,  1,  0,  0,  1,  0,  0,  0,  0,  0], 
[ 0,  0,  0,  1,  1,  0,  1, -4,  0,  0,  0,  1,  0,  0,  0,  0],
[ 0,  0,  0,  0,  1,  0,  0,  0, -4,  1,  0,  1,  1,  0,  0,  0],
[ 0,  0,  0,  0,  0,  1,  0,  0,  1, -4,  1,  0,  0,  1,  0,  0],
[ 0,  0,  0,  0,  0,  0,  1,  0,  0,  1, -4,  1,  0,  0,  1,  0],
[ 0,  0,  0,  0,  0,  0,  0,  1,  1,  0,  1, -4,  0,  0,  0,  1],
[ 1,  0,  0,  0,  0,  0,  0,  0,  1,  0,  0,  0, -4,  1,  0,  1],
[ 0,  1,  0,  0,  0,  0,  0,  0,  0,  1,  0,  0,  1, -4,  1,  0],
[ 0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  1,  0,  0,  1, -4,  1],
[ 0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  1,  1,  0,  0, -4]
])
'''
Ae = np.array([
-4,  1,  1,  1,  1, 
 1, -4,  1,  1,  1, 
 1, -4,  1,  1,  1, 
 1,  1, -4,  1,  1,
 1, -4,  1,  1,  1, 
 1,  1, -4,  1,  1, 
 1,  1, -4,  1,  1, 
 1,  1,  1, -4,  1,
 1, -4,  1,  1,  1,
 1,  1, -4,  1,  1,
 1,  1, -4,  1,  1,
 1,  1,  1, -4,  1,
 1,  1, -4,  1,  1,
 1,  1,  1, -4,  1,
 1,  1,  1, -4,  1,
 1,  1,  1,  0, -4
], 
dtype=float)

Aj = np.array([
 0,  1,  3,  4, 12, 
 0,  1,  2,  5, 13, 
 1,  2,  3,  6, 14, 
 0,  2,  3,  7, 15, 
 0,  4,  5,  7,  8, 
 1,  4,  5,  6,  9, 
 2,  5,  6,  7, 10, 
 3,  4,  6,  7, 11, 
 4,  8,  9, 11, 12, 
 5,  8,  9, 10, 13, 
 6,  9, 10, 11, 14, 
 7,  8, 10, 11, 15, 
 0,  8, 12, 13, 15, 
 1,  9, 12, 13, 14, 
 2, 10, 13, 14, 15, 
 3, 11, 12, 14, 15 
])

NAi = np.array([ 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80 ])

mA = csr_matrix((Ae, Aj, NAi))

N_X =16

v_df = np.empty((16, 1), dtype=float)

v_dummy = np.empty((N_proc, 1), dtype=float)

mX = np.zeros((16, 1), dtype=float)

mB= np.array([
[0.7], 
[0.1], 
[0.2], 
[0.1],
[0.0], 
[2.1], 
[0.6], 
[0.1],
[-0.1], 
[0.6], 
[0.2], 
[1.1],
[0.1], 
[3.1], 
[0.5],
[-0.2]
])

c=0

ii, fi = MPI_if(N_proc, proc_i, N_X)

N_proc_i = fi-ii+1
proc_ini = ii-1

v_tau_loc    = v_tau[proc_ini : fi]

v_dummy_loc = np.empty((1, 1), dtype=float)


sizes_X = np.empty(N_proc, dtype=int) # np.zeros(N_proc, dtype=int)
sizes_X_loc = np.array([N_proc_i])
comm.Allgatherv([sizes_X_loc, MPI.INTEGER], [sizes_X, MPI.INTEGER])

offsets_X = np.empty(N_proc, dtype=int)
acc_off =0 

for i in range(N_proc):
    offsets_X[i] = acc_off
    acc_off = acc_off + sizes_X[i]

NAi_loc = NAi[proc_ini:fi+1]
NAi_loc_0 = NAi[proc_ini:fi+1]-NAi[proc_ini]

Ae_loc  = Ae[NAi_loc[0]: NAi_loc[N_proc_i]]
Aj_loc  = Aj[NAi_loc[0]: NAi_loc[N_proc_i]]

mB_loc  = mB[proc_ini:fi]

mA_loc = csr_matrix((Ae_loc, Aj_loc, NAi_loc_0), shape=(N_proc_i,N_X))

diff = 1e100 
N_odd = 0

for i in range(0,3000):

    mX_loc  = mX[proc_ini:fi]    

    mA_mX_loc = mA_loc*mX
    
    old_f_loc = np.matmul(mX_loc.transpose(),mA_mX_loc)[0][0] 
    old_f     = comm.allreduce(old_f_loc, op=MPI.SUM)
    old_f     = 0.5 * old_f - np.matmul(mX.transpose(), mB)[0][0] + c 
           
    #comm.barrier()

    old_diff = diff     

    v_df_loc = mA_mX_loc - mB_loc    
    
    comm.Allgatherv([v_df_loc, MPI.DOUBLE], [v_df, sizes_X, offsets_X, MPI.DOUBLE])
   
    
    sum_df2_loc = (v_df_loc**2).sum()
    sum_adf = mA*v_df
    sum_adf_loc = sum_adf[proc_ini:fi]
    sum_adfdf_loc = (sum_adf_loc*v_df_loc).sum()
    
    sum_df2     = comm.allreduce(sum_df2_loc  , op=MPI.SUM)
    sum_adfdf   = comm.allreduce(sum_adfdf_loc, op=MPI.SUM)
    
    tau = sum_df2/sum_adfdf    
    
    mX_loc = mX_loc - tau * v_df_loc

    comm.Allgatherv([mX_loc, MPI.DOUBLE], [mX, sizes_X, offsets_X, MPI.DOUBLE])

    mX_loc  = mX[proc_ini:fi]    
    f_loc = np.matmul(mX_loc.transpose(),mA_loc*mX)[0][0] 
    f     = comm.allreduce(f_loc, op=MPI.SUM)
    f     = 0.5 * f - np.matmul(mX.transpose(), mB)[0][0] + c 

    diff = abs(f-old_f)
    
    #if proc_i==0 and i<3000: print("{0:3d} {1:6f} {2:6f} {3:6f} {4:6f} {5:6e} {6:6e} {7:6e}".format(i, mX[0][0], mX[1][0], mX[2][0], mX[3][0], diff, f, old_f))
    
    if diff < Tol : break

end_time = datetime.datetime.now()

elapsed_time = end_time - start_time

micro_elapsed_time = elapsed_time.microseconds

if proc_i==0:    
    print("{0:3d} {1:6f} {2:6f} {3:6f} {4:6f} {5:6e}".format(i, mX[0][0], mX[1][0], mX[2][0], mX[3][0], diff))
    print('elapsed time', micro_elapsed_time, 'ms')

MPI.Finalize() 