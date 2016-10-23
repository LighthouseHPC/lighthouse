README
1. make properties_moose.c
2. Test one matrix by running: 
./properties_moose -f data/mat_files/twophasestress_1_1_1.mat 
3. python compute_32_feat.py
4. sh properties_script.sh
Result file: properties_results.csv

For generating arff/csv with PETSc features for moose matrices(scaled):
1. In the properties csv file (properties_moose_1e4_6080.csv), move matrix name from last column to first column.
2. Check for  character in the csv file, if found, remove by doing the following in vim: 
:%s//\r/g
(Note: to type  you should NOT use power and capital M, instead press Ctrl (V and M)) 
3. Rename log file extensions from abc.p24.log to abc.log. Use script fileExtensionChange.py
4. From location: /Users/kanikas/Documents/github/Lighthouse_recent/lighthouse/sandbox/ml
run the command: 
python ../scripts/petsc2arff_moose.py -T ../petsc/moose_big_matrices/petsc-moose-features/properties_moose_1e4_6080.csv -p ../petsc/moose_big_matrices/moose-timing-logs -t -b 30 -n solvers_moose_petsc

To remove logs with iterations <10: 
From location: /Users/kanikas/Documents/github/Lighthouse_recent/lighthouse/sandbox/petsc/moose_big_matrices/timing-logs-backup
Run command: grep -lir "its=1$"  *.log | xargs -I {} cp {} ../iter_geq10/
for its = 1...9
