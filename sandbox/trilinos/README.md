# README #

# Contact for this section #
Pate Motter
* pate.motter@colorado.edu

# Basic idea #
* This work involves adding support and testing for the Trilinos library to Lighthouse
* The "Solvers" directory is used for computing the permutations of Trilinos' Belos and Ifpack2 packages
* The "Properties" directory is used for determining matrix properties similar to the AnaMod library using Trilinos
* Both programs use the Tpetra Trilinos library for the common data structures (as opposed to Epetra)

# Tpetra Solvers #
* Used for determining the results and timing information associated with solving a matrix in Matrix-Market format (.mtx)
* Permutes the available preconditioners and solvers available within the Belos and Ifpack2 Trilinos packages
* Compile: `./do-configure`
* Make: `make`
* Usage: `./tpetra_solvers <mtx file> [<output directory>]`
  * The output directory is optional and if not included the results will be printed to screen
  * `parse2csv.py` can be used to produce a more readable file from the results
  
# Tpetra Properties #
* Outputs various spectral, normative, etc. properties via Trilinos. Mimics the functionality of AnaMod
* Compile: `./do-configure`
* Make: `make`
* Usage: `./tpetra_properties <mtx file> [<output directory>]`
  * The output directory is optional and if not included the results will be printed to screen
  * `parse2csv.py` can be used to produce a more readable file from the resulting properties
