"""Similar to LV1.py however takes parameter values as user input through terminal and saves result in PDF in Results"""

__author__ = 'Matthew Campos (matthew.campos19@imperial.ac.uk)'
__version__ = '0.0.1'

def main(argv):
    """ Main entry point of the program """
    import scipy as sc
    import scipy.integrate as integrate
    import matplotlib.pylab as p

    def dCR_dt(pops, t=0):
        """returns the growth rate of consumer and resource population at any given time step"""

        R = pops[0]
        C = pops[1]
        K = 10000000
        dRdt = r * R * (1-(R/K)) - a * R * C
        dCdt = -z * C + e * a * R * C

        return sc.array([dRdt, dCdt])

    #integrate from time point 0 to 15, using 1000 sub-divisions of time
    t = sc.linspace(0, 15, 1000)

    def plot(pops):
        f1 = p.figure()
        p.plot(t, pops[:,0], 'g-', label='Resource density') # Plot
        p.plot(t, pops[:,1]  , 'b-', label='Consumer density')
        p.grid()
        p.legend(loc='best')
        p.xlabel('Time')
        p.ylabel('Population density')
        p.title('Consumer-Resource population dynamics')
        #p.show()# To display the figure
        f1.savefig('../Results/LV2_model.pdf')

        f2 = p.figure()
        p.plot(pops[:,0], pops[:,1], 'r-', label='Consumer density') # Plot
        p.grid()
        p.xlabel('Resource Density')
        p.ylabel('Consumer density')
        p.title('Consumer-Resource population dynamics')
        #p.show()# To display the figure
        f2.savefig('../Results/LV2_second_model.pdf')

        return f1,f2

    if len(argv) < 5:
        r = 1
        a = 0.1
        z = 1.5
        e = 0.75
    else:
        r,a,z,e = float(sys.argv[1]), float(sys.argv[2]), float(sys.argv[3]), float(sys.argv[4])
    #integrate from time point 0 to 15, using 1000 sub-divisions of time
    t = sc.linspace(0, 15, 1000)
    #Set the initial conditions for the two populations
    R0 = 10
    C0 = 5
    RC0 = sc.array([R0, C0])
    #numerically integrate this system forward from those starting conditions
    pops, infodict = integrate.odeint(dCR_dt, RC0, t, full_output=True)
    print("final Consumer and Resource population values are:", pops[-1]) #prints final values
    plot(pops)
    return 0

if __name__ == "__main__":
    """Makes sure the main function is called from command line"""
    import sys
    status = main(sys.argv)
    sys.exit(status)
