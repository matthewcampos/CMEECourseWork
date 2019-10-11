taxa = [ ('Myotis lucifugus','Chiroptera'),
         ('Gerbillus henleyi','Rodentia',),
         ('Peromyscus crinitus', 'Rodentia'),
         ('Mus domesticus', 'Rodentia'),
         ('Cleithrionomys rutilus', 'Rodentia'),
         ('Microgale dobsoni', 'Afrosoricida'),
         ('Microgale talazaci', 'Afrosoricida'),
         ('Lyacon pictus', 'Carnivora'),
         ('Arctocephalus gazella', 'Carnivora'),
         ('Canis lupus', 'Carnivora'),
        ]

# Write a short python script to populate a dictionary called taxa_dic
# derived from  taxa so that it maps order names to sets of taxa.
# E.g. 'Chiroptera' : set(['Myotis lucifugus']) etc.
order=set()
for i in taxa:
    order.add(i[1]) # creates set of order to eliminate repetition

taxa_dict=dict.fromkeys(order) #makes a new dictionary using Order as a key with no values
for order in taxa_dict.keys():
    values=[] # creates a list of the many species per order
    for y in taxa:
        if y[1]==order:
            values.append(y[0]) # adds to the list without replacing value each loop
    taxa_dict[order]=values # adds values to each key
print(taxa_dict)
