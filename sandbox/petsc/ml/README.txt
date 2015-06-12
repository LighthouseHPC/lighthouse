The petsc solvers .arff file (Weka input) is generated with 

* For Anamod features + PETSc linear solvers:
../scripts/anamod2arff.py -f ../anamod-features -p ../timing/bgq -n solvers_anamod

* For TPetra (Trilinos) features + PETSc linear solvers:
../scripts/anamod2arff.py -T ../../trilinos/tpetra_properties/12procs_results.csv -p ../timing/bgq -n solvers_tpetra

----

The Belos (Trilinos) arff file can be generated with:

* For Anamod features + Trilinos linear solvers:
../scripts/anamod2arff.py -f ../anamod-features -B ../timing/janus-belos/belos_ifpack2_results.csv -n belos_anamod

* For TPetra (Trilinos) features + Trilinos linear solvers:
../scripts/anamod2arff.py -T ../../trilinos/tpetra_properties/12procs_results.csv -B ../timing/janus-belos/belos_ifpack2_results.csv -n belos_tpetra
