#
# CMake example that uses FIND_PACKAGE(Trilinos ...) to build your C++
# application with Trilinos.  You should know a little bit about CMake
# before reading this example; in particular, you should know how to
# add C++ source files and header files to your project.
#

# You need CMake version >= 2.8 to use this example.
cmake_minimum_required(VERSION 2.8)

# Your "do-configure" script that invokes CMake should set
# TRILINOS_PATH to the path to your Trilinos install.
# You do _not_ need to edit this line.
FIND_PACKAGE(Trilinos PATHS ${TRILINOS_PATH}/lib/cmake/Trilinos ${TRILINOS_PATH})

# If FIND_PACKAGE successfully found your Trilinos install, it will
# set the Boolean flag Trilinos_FOUND.  The following IF statement
# fails with a FATAL_ERROR if Trilinos was not found.  If it _was_
# found, it prints out the values of some Trilinos configuration
# details.  You may find them useful for building your application
# that uses Trilinos.
IF(Trilinos_FOUND)
   MESSAGE("\nFound Trilinos!  Here are the details: ")
   MESSAGE("   Trilinos_DIR = ${Trilinos_DIR}")
   MESSAGE("   Trilinos_VERSION = ${Trilinos_VERSION}")
   MESSAGE("   Trilinos_PACKAGE_LIST = ${Trilinos_PACKAGE_LIST}")
   MESSAGE("   Trilinos_LIBRARIES = ${Trilinos_LIBRARIES}")
   MESSAGE("   Trilinos_INCLUDE_DIRS = ${Trilinos_INCLUDE_DIRS}")
   MESSAGE("   Trilinos_TPL_LIST = ${Trilinos_TPL_LIST}")
   MESSAGE("   Trilinos_TPL_INCLUDE_DIRS = ${Trilinos_TPL_INCLUDE_DIRS}")
   MESSAGE("   Trilinos_TPL_LIBRARIES = ${Trilinos_TPL_LIBRARIES}")
   MESSAGE("   Trilinos_BUILD_SHARED_LIBS = ${Trilinos_BUILD_SHARED_LIBS}")
   MESSAGE("   Trilinos_CXX_COMPILER = ${Trilinos_CXX_COMPILER}")
   MESSAGE("   Trilinos_C_COMPILER = ${Trilinos_C_COMPILER}")
   MESSAGE("   Trilinos_Fortran_COMPILER = ${Trilinos_Fortran_COMPILER}")
   MESSAGE("   Trilinos_CXX_COMPILER_FLAGS = ${Trilinos_CXX_COMPILER_FLAGS}")
   MESSAGE("   Trilinos_C_COMPILER_FLAGS = ${Trilinos_C_COMPILER_FLAGS}")
   MESSAGE("   Trilinos_Fortran_COMPILER_FLAGS =
     ${Trilinos_Fortran_COMPILER_FLAGS}")
   MESSAGE("   Trilinos_LINKER = ${Trilinos_LINKER}")
   MESSAGE("   Trilinos_EXTRA_LD_FLAGS = ${Trilinos_EXTRA_LD_FLAGS}")
   MESSAGE("   Trilinos_AR = ${Trilinos_AR}")
   MESSAGE("End of Trilinos details\n")
ELSE()
  MESSAGE(FATAL_ERROR "Could not find Trilinos!")
ENDIF()

# If you haven't already set the C++ compiler, use the same compiler
# that was used to build Trilinos on your machine.  You do _not_ need
# to edit this line.
IF(NOT CMAKE_CXX_COMPILER)
  MESSAGE("Setting CXX compiler.")
  SET(CMAKE_CXX_COMPILER ${Trilinos_CXX_COMPILER})
ENDIF()

# If you haven't already set the C compiler, use the same compiler
# that was used to build Trilinos on your machine.  You do _not_ 
# need to edit this line.
IF(NOT CMAKE_C_COMPILER)
  MESSAGE("Setting C compiler.")
  SET(CMAKE_C_COMPILER ${Trilinos_C_COMPILER})
ENDIF()

#
# EDIT HERE: Replace TrilinosExample with your project's name.
#
project(tpetra_testing)
# 
# EDIT HERE: 
#
# Source file(s) in your project.  Here, we include one source file, 
# main.cpp, which contains the main() function.  You may also want to 
# set HEADERS to the list of header files in your project, if there 
# are any.
#
set(SOURCES 
		tpetra_testing.cpp
  )

# 
# EDIT HERE:
#
# Add to the header include path the current directory, Trilinos'
# include directory, and the include directories of the third-party
# libraries (TPLs) with which Trilinos was built.
#
include_directories(".")
include_directories(${Trilinos_TPL_INCLUDE_DIRS})
include_directories(${Trilinos_INCLUDE_DIRS})

# Add to the library path Trilinos' library path, and the library
# paths of the third-party libraries (TPLs) with which Trilinos was
# built.  
link_directories(${Trilinos_LIBRARY_DIRS})
link_directories(${Trilinos_TPL_LIBRARY_DIRS})

# 
# EDIT HERE: Change trilinosExample to the name of the executable
# file that you want to build.  This line tells CMake which source
# file(s) to use to build the executable.
#
add_executable(tpetra_testing ${SOURCES})

#
# EDIT HERE: Change trilinosExample to the name of the executable
# file, as above.  This line tells the linker to link the executable 
# with the Trilinos libraries, and with the third-party libraries 
# (TPLs) with which Trilinos was built.
#
target_link_libraries(tpetra_testing
  ${Trilinos_LIBRARIES} 
  ${Trilinos_TPL_LIBRARIES}
  )

