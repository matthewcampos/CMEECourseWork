import networkx as nx
import scipy as sc
import matplotlib.pyplot as p
import csv
import pandas as pd
import numpy as np
import matplotlib.patches as patches

#loc gets rows (or columns) with particular labels from the index.
#iloc gets rows (or columns) at particular positions in the index (so it only takes integers).

# 	ICL	UoR	CEH	ZSL	CEFAS	Nonacademic/CASE
# ICL	0	0	10	9	5	70
# UoR		0	12	0	2	76
# CEH			0	0	0	6
# ZSL				0# plot(net, edge.arrow.size=1, edge.curved=.1,
#      vertex.color="orange", vertex.frame.color="#555555",
#      vertex.label=V(net)$Type, vertex.label.color="black",
#      vertex.label.cex=.7) 	0	28
# CEFAS					0	0
# Nonacademic/CASE						0
#link_sc = sc.genfromtxt("../Data/QMEE_Net_Mat_edges.csv", delimiter=",")
#nodes_sc = sc.genfromtxt("../Data/QMEE_Net_Mat_nodes.csv", delimiter=",", dtype=str)

link_pd = pd.read_csv("../Data/QMEE_Net_Mat_edges.csv", header=0)
link_pd.columns = list(range(0,6))
print(link_pd)
nodes_pd = pd.read_csv("../Data/QMEE_Net_Mat_nodes.csv", header=0)
print(nodes_pd)
print(nodes_pd['Type'][0])

#create colour list
colour = []
for k in range(6):
    if nodes_pd['Type'][k]==str("University"):
        colour.append(str("blue"))
    if nodes_pd['Type'][k]==str("Hosting Partner"):
        colour.append(str("green"))
    if nodes_pd['Type'][k]==str("Non-Hosting Partners"):
        colour.append(str("red"))
print(colour)

#create list of tuples with values > 0
link_pd_list = []
weights = []
for i in range(6):
    for j in range(6):
        if link_pd.iloc[i][j] > 0:
            link_pd_list.append((i,j)) #add coordinates to list
            weights.append(int(link_pd[i][j])/10) #and weights
print(link_pd_list)

pos = nx.circular_layout(nodes_pd.index.values) #creates canvas
G = nx.DiGraph() #initiate graph with arrows in edges
G.add_nodes_from(nodes_pd.index.values) #add nodes
G.add_edges_from(link_pd_list, arrows=True) #add edges

#Initialise figure
plot = p.figure()

#Draw the plot
nx.draw_networkx(G, pos, node_size=2000, with_labels=False, width=weights, node_color=colour, edge_color="grey")
#add labels
nx.draw_networkx_labels(G, pos, nodes_pd["id"]) #labelling each node

#legend
blue = patches.Patch(color = "b", label="University")
green = patches.Patch(color = "g", label="Housing Partner")
red = patches.Patch(color = "r", label="Non-Hosting Partner")
p.legend(handles=[blue,green,red])

plot.savefig("../Results/Nets_py.svg")
