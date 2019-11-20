import networkx as nx
import scipy as sc
import matplotlib.pyplot as p

#capture information using adjacency list and node list to create network
#adjacency list describes interaction of individuals in system i.e. predator prey interaction
#node list describes individual properties i.e. body mass

def GenRdmAdjList(N = 2, C = 0.5): #C is probability of interaction
    """Function that generates random adjacent list"""
    Ids = range(N) #species
    ALst = []
    for i in Ids:
        if sc.random.uniform(0,1,1) < C: #uniform distribution generating 1 random value. Cumulative probability
            Lnk = sc.random.choice(Ids,2).tolist() #picks 2 IDs at random and coerce into a list
            if Lnk[0] != Lnk[1]: #avoid cannibalism as it samples with replacement
                ALst.append(Lnk)
    return ALst

MaxN = 30
C = 0.75

AdjL = sc.array(GenRdmAdjList(MaxN,C))
print(AdjL)

Sps = sc.unique(AdjL) # get species ids...python version of unique
print(Sps)

SizRan = ([-10,10]) #use log10 scale
Sizs = sc.random.uniform(SizRan[0],SizRan[1],MaxN) #range of log(10)10**-10 to log(10)10**10
f1 = p.figure()
p.hist(Sizs) #log10 scale
f2 = p.figure()
p.hist(10 ** Sizs) #raw scale
p.show()


f3 = p.figure()
pos = nx.circular_layout(Sps)
G = nx.Graph()
G.add_nodes_from(Sps)
G.add_edges_from(tuple(AdjL))
NodSizs= 1000 * (Sizs-min(Sizs))/(max(Sizs)-min(Sizs))
nx.draw_networkx(G, pos, node_size = NodSizs, node_color = "red")
p.show()
f3.savefig('../Results/DrawFW_network_model.pdf')
p.close('all')
