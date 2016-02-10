# Run this in the directory containing PETSc logs to get a quick count on what solvers are 
# tested and how many matrices per solver
ls -l | awk '{print($9);}' | tr '.' ' ' | awk '{print($2);}' | sort | uniq
