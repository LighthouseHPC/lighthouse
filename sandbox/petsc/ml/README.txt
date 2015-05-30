The petsc.arff file (Weka input) is generated with 

* For Anamod features + PETSc linear solvers:
../scripts/anamod2arff.py -f ../anamod-features -p ../timing/bgq -n solvers_anamod

* For TPetra (Trilinos) features + PETSc linear solvers:
../scripts/anamod2arff.py -T ../../trilinos/tpetra_properties/12procs_results.csv -p ../timing/bgq -n solvers_tpetra
