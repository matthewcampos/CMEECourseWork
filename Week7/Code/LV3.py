import scipy as sc
import sys
import matplotlib.pylab as p

def CR_t(RC0, t=0):
    """returns the growth rate of consumer and resource population at any given time step"""
    RC = sc.zeros((t,2), dtype = 'float')
    RC[0,0] = RC0[0]
    RC[0,1] = RC0[1]
    for i in range(t-1):
        RC[i+1,0] = RC[i,0] * (1 + r * (1 - (RC[i,0]/K)) - a * RC[i,1])
        RC[i+1,1] = RC[i,1] * (1 - z + (e * a * RC[i,0]))
        if RC[i+1,1] < 0:
            RC[i+1,1] = 0
            print("resource population went extinct at time", i)
            break

    return RC

if len(sys.argv)==6:
    K = float(sys.argv[1])
    r = float(sys.argv[2])
    a = float(sys.argv[3])
    z = float(sys.argv[4])
    e = float(sys.argv[5])
else:
    K = 30
    r = 1.
    a = 0.1
    z = 1.2
    e = 0.75


#integrate from time point 0 to 15, using 1000 sub-divisions of time
t = 100
R0 = 10
C0 = 5

RC0 = sc.array([R0,C0], dtype = 'float')
RC = CR_t(RC0,t)
print("consumer population is: %s, resource population is: %s" %(RC[t-1,1],RC[t-1,0]))

print(RC)

f1 = p.figure()
p.plot(range(t), RC[:,0], 'g-', label='Resource density') # Plot
p.plot(range(t), RC[:,1]  , 'b-', label='Consumer density')
p.grid()
p.legend(loc='best')
p.xlabel('Time')
p.ylabel('Population density')
p.title('Consumer-Resource population dynamics')
#p.show()# To display the figure
f1.savefig('../Results/LV3_model.pdf')

f2 = p.figure()
p.plot(RC[:,0], RC[:,1], 'r-', label='Consumer density') # Plot
p.grid()
p.xlabel('Resource Density')
p.ylabel('Consumer density')
p.title('Consumer-Resource population dynamics')
#p.show()# To display the figure
f2.savefig('../Results/LV3_second_model.pdf')
p.close('all')
