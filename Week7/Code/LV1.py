import scipy as sc
import scipy.integrate as integrate

def dCR_dt(pops, t=0):
     """returns the growth rate of consumer and resource population at any given time step"""
     R = pops[0]
     C = pops[1]
     dRdt = r * R - a * R * C
     dCdt = -z * C + e * a * R * C

     return sc.array([dRdt, dCdt])

#assign some parameter values
r = 1.
a = 0.1
z = 1.5
e = 0.75

#integrate from time point 0 to 15, using 1000 sub-divisions of time
t = sc.linspace(0, 15, 1000)

#Set the initial conditions for the two populations
R0 = 10
C0 = 5
RC0 = sc.array([R0, C0])

#numerically integrate this system forward from those starting conditions
pops, infodict = integrate.odeint(dCR_dt, RC0, t, full_output=True)
print("final Consumer and Resource population values are:", pops[-1]) #prints final values

type(infodict)
infodict.keys()
infodict['message']

import matplotlib.pylab as p

f1 = p.figure()

p.plot(t, pops[:,0], 'g-', label='Resource density') # Plot
p.plot(t, pops[:,1]  , 'b-', label='Consumer density')
p.grid()
p.legend(loc='best')
p.xlabel('Time')
p.ylabel('Population density')
p.title('Consumer-Resource population dynamics')
#p.show()# To display the figure

f1.savefig('../Results/LV_model.pdf')

f2 = p.figure()

p.plot(pops[:,0], pops[:,1], 'r-', label='Consumer density') # Plot
p.grid()
p.xlabel('Resource Density')
p.ylabel('Consumer density')
p.title('Consumer-Resource population dynamics')
#p.show()# To display the figure

f2.savefig('../Results/LV_second_model.pdf')
