import os
parentdir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
os.sys.path.insert(0,parentdir) 


#### sort the solve routines and find driver_simple, driver_expert, and others and
#### save the routines in routines/solve_simple.txt and routines/solve_expert.txt

print "Sort the 'solve' routines to find driver_simple and driver_expert routines."

f_solve_single = open('./routines/solve_341_single.txt')
f_solve_double = open('./routines/solve_341_double.txt')
f_solve_complex = open('./routines/solve_341_complex.txt')
f_solve_complex16 = open('./routines/solve_341_complex16.txt')

f_simple = open('./routines/solve_simple.txt', 'w')
f_expert = open('./routines/solve_expert.txt', 'w')

driver_simple = []
driver_expert = []
others = []


def sortSolve(fileName):
    for line in fileName:
        routineName = line.split("/")[-1]
        if "sv.f" in routineName:
            driver_simple.append(routineName)
            f_simple.write(routineName)
        elif "svx" in routineName:
            driver_expert.append(routineName)
            f_expert.write(routineName)
        else:
            others.append(routineName)
            print routineName
    fileName.close()



sortSolve(f_solve_single)
sortSolve(f_solve_double)
sortSolve(f_solve_complex)
sortSolve(f_solve_complex16)


#check in the computational group
f_should_be_driver = open(parentdir+'/computational_solve/routines/driver_341.txt')
print "The following simple routines should NOT be in the computational group:"
i = 0
for routineName in f_should_be_driver:
    if "sv.f" in routineName:
        driver_simple.append(routineName)
        f_simple.write(routineName)
    elif "svx" in routineName:
        driver_expert.append(routineName)
        f_expert.write(routineName)
    else:
        others.append(routineName)
        print routineName

f_should_be_driver.close()
    
f_simple.close()
f_expert.close()

print "Driver simple: %s" % len(driver_simple)
print "Driver expert: %s" % len(driver_expert)
print "others: %s" % len(others)