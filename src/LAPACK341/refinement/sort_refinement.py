### ---- Sort routines/refinement_341.txt based on the last few charactors ---- ###
f_refinement_341 = open('./routines/refinement_341.txt', 'r')
f_refinement_341_sorted = open('./routines/refinement_341_sorted.txt', 'w')

refinement_list = []
keys = []
for line in f_refinement_341:
    routine = line.rstrip('.f\r\n')
    aKey = routine[-2:]
    refinement_list.append(routine)
    keys.append(aKey)

keys = list(set(keys))

print keys

for element in keys:
    i = 0
    for routine in refinement_list:
        if routine[-2:]==element:
            i += 1
            f_refinement_341_sorted.write(routine +'\n')
    f_refinement_341_sorted.write("--------------------- %s \n\n" % i)
    


f_refinement_341_sorted.close()
f_refinement_341.close()

