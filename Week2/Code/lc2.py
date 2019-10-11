# Average UK Rainfall (mm) for 1910 by month
# http://www.metoffice.gov.uk/climate/uk/datasets
rainfall = (('JAN',111.4),
            ('FEB',126.1),
            ('MAR', 49.9),
            ('APR', 95.3),
            ('MAY', 71.8),
            ('JUN', 70.2),
            ('JUL', 97.1),
            ('AUG',140.2),
            ('SEP', 27.0),
            ('OCT', 89.4),
            ('NOV',128.4),
            ('DEC',142.2),
           )

# (1) Use a list comprehension to create a list of month,rainfall tuples where
# the amount of rain was greater than 100 mm.
l_rain=[i[0] for i in rainfall if i[1]<50]
print(l_rain)

# (2) Use a list comprehension to create a list of just month names where the
# amount of rain was less than 50 mm.
h_rain=[i[0] for i in rainfall if i[1]>100]
print(h_rain)

# (3) Now do (1) and (2) using conventional loops (you can choose to do
# this before 1 and 2 !).
heavy_rain=[] #retrieves months of rain greater than 100mm from the data
for i in rainfall:
    if i[1]>100: #conditional
        heavy_rain.append(i[0]) #since it is a list, use the append
print(heavy_rain)

light_rain=[] #retrieves months of light rain, less than 50mm from the data
for i in rainfall:
    if i[1]<50: #conditional
        light_rain.append(i[0])
print(light_rain)
