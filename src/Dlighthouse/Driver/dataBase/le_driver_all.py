import csv


###----- Created le_driver_all.csv based on le_simple.csv and le_expert.csv -----###

f_le_simple = csv.reader(open('le_simple.csv', 'rb'), delimiter=',')
f_le_expert = csv.reader(open('le_expert.csv', 'rb'), delimiter=',')
f_le_driver_all = csv.writer(open('le_driver_all.csv', 'wb'), delimiter=',')

i = 1
for row in f_le_simple:
    i = row[0]
    f_le_driver_all.writerow(row)
    


for row in f_le_expert:
    row[0] = int(row[0])+ int(i)
    row[0] = str(row[0])
    f_le_driver_all.writerow(row)    