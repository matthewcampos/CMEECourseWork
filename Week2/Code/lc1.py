birds = ( ('Passerculus sandwichensis','Savannah sparrow',18.7),
          ('Delichon urbica','House martin',19),
          ('Junco phaeonotus','Yellow-eyed junco',19.5),
          ('Junco hyemalis','Dark-eyed junco',19.6),
          ('Tachycineata bicolor','Tree swallow',20.2),
         )

#(1) Write three separate list comprehensions that create three different
# lists containing the latin names, common names and mean body masses for
# each species in birds, respectively.
latin_names=[i[0] for i in birds] #i[0] retrieves the value in the first index of each row
print(latin_names)
common_names=[i[1] for i in birds] #i[1] retrieves the value from the second index in each row
print(common_names)
mean_body_masses=[i[2] for i in birds] #i[2] retrieves the third value
print(mean_body_masses)

# (2) Now do the same using conventional loops (you can choose to do this
# before 1 !).
latin_names=[]
common_names=[]
mean_body_masses=[]
for i in birds:
    latin_names.append(i[0]) #these are lists so use the append and
    common_names.append(i[1])
    mean_body_masses.append(i[2])
print(latin_names)
print(common_names)
print(mean_body_masses)
