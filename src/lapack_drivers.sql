DROP TABLE IF EXISTS Problem;
DROP TABLE IF EXISTS Linear_Equation;
DROP TABLE IF EXISTS Linear_Least_Squares;
DROP TABLE IF EXISTS Sym_Eigen;
DROP TABLE IF EXISTS nonSym_Eigen;
DROP TABLE IF EXISTS SVD;
DROP TABLE IF EXISTS routineList;
DROP FUNCTION IF EXISTS callURL;



/*
create table Problem:
si = simple
ex = expert
*/
CREATE TABLE Problem(
id_p SMALLINT UNSIGNED NOT NULL AUTO_INCREMENT,
problem VARCHAR(100),
PRIMARY KEY(id_p)
); 

/*
Insert values into table Problem
*/
LOAD DATA LOCAL INFILE "/sandbox/salin/Documents/Lighthouse/Driver/problem.csv" INTO TABLE Problem; 



/*
create table Linear_Equation (80 routines):
s=single
d=double
c=complex single
z=complex double

f=full
b=banded
p=packed
t=tridiagonal
g=generalized problem
*/
CREATE TABLE Linear_Equation(
id_le SMALLINT UNSIGNED NOT NULL AUTO_INCREMENT,
thePrecision ENUM('s', 'd', 'c', 'z'),
routineName VARCHAR(6) NOT NULL,
matrixType VARCHAR(20) NOT NULL,  
structureType ENUM('f', 'b', 'p', 't', 'g'),
url VARCHAR(20),
problem SMALLINT UNSIGNED NOT NULL,
description VARCHAR(255),
PRIMARY KEY (id_le),
FULLTEXT (routineName, matrixType, url, description)
);


/*
Insert values into table Linear_Equation
*/
LOAD DATA LOCAL INFILE "/sandbox/salin/Documents/Lighthouse/Driver/le.csv" INTO TABLE Linear_Equation; 



/*
create table Linear_Least_Squares (24 routines).
*/
CREATE TABLE Linear_Least_Squares(
id_lls SMALLINT UNSIGNED NOT NULL AUTO_INCREMENT,
thePrecision ENUM('s', 'd', 'c', 'z'),
routineName VARCHAR(6) NOT NULL,
matrixType VARCHAR(20) NOT NULL,  
structureType ENUM('f', 'b', 'p', 't', 'g'),
url VARCHAR(20),
problem SMALLINT UNSIGNED NOT NULL,
description VARCHAR(255),
PRIMARY KEY (id_lls),
FULLTEXT (routineName, matrixType, url, description)
);

/*
Insert values into table Linear_Least_Squares
*/
LOAD DATA LOCAL INFILE "/sandbox/salin/Documents/Lighthouse/Driver/lls.csv" INTO TABLE Linear_Least_Squares; 



/*
create table Sym_Eigen (84 routines).
*/
CREATE TABLE Sym_Eigen(
id_sei SMALLINT UNSIGNED NOT NULL AUTO_INCREMENT,
thePrecision ENUM('s', 'd', 'c', 'z'),
routineName VARCHAR(6) NOT NULL,
matrixType VARCHAR(20) NOT NULL,  
structureType ENUM('f', 'b', 'p', 't', 'g'),
url VARCHAR(20),
problem SMALLINT UNSIGNED NOT NULL,
description VARCHAR(255),
PRIMARY KEY (id_sei),
FULLTEXT (routineName, matrixType, url, description)
); 

/*
Insert values into table Sym_Eigen
*/
LOAD DATA LOCAL INFILE "/sandbox/salin/Documents/Lighthouse/Driver/seig.csv" INTO TABLE Sym_Eigen; 



/*
create table nonSym_Eigen (32 routines).
*/
CREATE TABLE nonSym_Eigen(
id_non SMALLINT UNSIGNED NOT NULL AUTO_INCREMENT,
thePrecision ENUM('s', 'd', 'c', 'z'),
routineName VARCHAR(6) NOT NULL,
matrixType VARCHAR(20) NOT NULL,  
structureType ENUM('f', 'b', 'p', 't', 'g'),
url VARCHAR(20),
problem SMALLINT UNSIGNED NOT NULL,
description VARCHAR(255),
PRIMARY KEY (id_non),
FULLTEXT (routineName, matrixType, url, description)
); 

/*
Insert values into table nonSym_Eigen
*/
LOAD DATA LOCAL INFILE "/sandbox/salin/Documents/Lighthouse/Driver/nonseig.csv" INTO TABLE nonSym_Eigen; 




/*
create table SVD (12 routines).
*/
CREATE TABLE SVD(
id_svd SMALLINT UNSIGNED NOT NULL AUTO_INCREMENT,
thePrecision ENUM('s', 'd', 'c', 'z'),
routineName VARCHAR(6) NOT NULL,
matrixType VARCHAR(20) NOT NULL,  
structureType ENUM('f', 'b', 'p', 't', 'g'),
url VARCHAR(20),
problem SMALLINT UNSIGNED NOT NULL,
description VARCHAR(255),
PRIMARY KEY (id_svd),
FULLTEXT (routineName, matrixType, url, description)
); 




/*
Insert values into table SVD
*/
LOAD DATA LOCAL INFILE "/sandbox/salin/Documents/Lighthouse/Driver/svd.csv" INTO TABLE SVD; 





/*
Create a table containing all the routine names, url's, and their information file names
*/

CREATE TABLE routineList(
id SMALLINT UNSIGNED NOT NULL,
thePrecision ENUM('s', 'd', 'c', 'z'),
routineName VARCHAR(6) NOT NULL, 
url VARCHAR(20),
fileName VARCHAR(20)
);



/*
Insert values into table routineList
*/
LOAD DATA LOCAL INFILE "/sandbox/salin/Documents/Lighthouse/Driver/Routines/routineList.csv" INTO TABLE routineList; 

-- =========================================================================================

/*
create a function to print complete URL's --> callURL(url)
*/

SET @lapack = 'http://www.netlib.org/lapack/';


CREATE FUNCTION callURL(url VARCHAR(20)) 
RETURNS VARCHAR(225) 
RETURN CONCAT(@lapack,url);


/*
SELECT id_le, precision, routine, callURL(url) AS URL FROM Linear_Equation; 
*/


-- ========================================================================================

