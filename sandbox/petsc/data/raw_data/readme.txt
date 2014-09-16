---------------------------------------------------------------------------------------------------------------------
parallel_data.zip: Contains the raw data of various solver performance (using 4 processes in a single node). Each file contains 28 lines. The first line is the link to the matrix. Rest of the lines shows performances of different methods. Lines that contain only the KSP and PC names but no other information indicates that the KSP and PC failed to solve the problem. Other lines have 6 fields.

KSP | PC | PETSc converge reason | Execution time | Preconditioned residual norm | number of iterations

---------------------------------------------------------------------------------------------------------------------
sequential_data.zip: Contains the raw data of various solver performance (using 1 process). Each file contains 52 lines. The first line is the link to the matrix. Rest of the lines shows performances of different methods. If a method did not fail it shows the following information in a line.

KSP | PC | PETSc converge reason | Execution time | Preconditioned residual norm | number of iterations
---------------------------------------------------------------------------------------------------------------------
matrix_features.zip: Contains the features of the matrices.