f_read = open('./routines/pivot_growth_factor_341.txt', 'r')
f_write = open('./routines/pivot_growth_factor_341_sort.txt', 'w')

routines = []
keys = []
for line in f_read:
    routines.append(line)
    keyWord = line.rstrip('.f\r\n')[-2:]
    keys.append(keyWord)
    
    
keys = list(set(keys))

routines = list(set(routines))

for element in keys:
    i = 0
    for item in routines:
        if item.rstrip('.f\r\n')[-2:] == element:
            i += 1
            f_write.write(item)
    f_write.write("--------------------- %s \n\n"%i)
    
    
f_write.close()
f_read.close()
