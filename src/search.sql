DROP PROCEDURE IF EXISTS tableAll;
DROP PROCEDURE IF EXISTS tableProc;
DROP PROCEDURE IF EXISTS tableMatch;
DROP PROCEDURE IF EXISTS mySearch;


/*
Creat procedure tableAll that selects all information and usees variable as table names.
*/

delimiter $$

CREATE PROCEDURE tableAll(
       	tableName	VARCHAR(50)
)

BEGIN

SET @showTable = CONCAT_ws(" ",'SELECT T.thePrecision, T.routineName, T.matrixType, T.structureType, T.url, P.problem, T.description FROM', tableName, 'AS T JOIN Problem AS P WHERE T.problem = P.id_p');


PREPARE stmt_all FROM @showTable;
EXECUTE stmt_all;
DEALLOCATE PREPARE stmt_all;

END$$

delimiter ;	

-- =========================================================================================	

/*
create procedure callTable that has a condition.
*/

delimiter $$

CREATE PROCEDURE tableProc(
       	tableName	VARCHAR(50),
	cond		VARCHAR(200)
)

BEGIN

SET @searchTable = CONCAT_ws(" ",'SELECT T.thePrecision, T.routineName, T.matrixType, T.structureType, T.url, P.problem, T.description FROM', tableName, 'AS T JOIN Problem AS P WHERE T.problem = P.id_p AND', cond);


PREPARE stmt FROM @searchTable;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;

END$$

delimiter ;	

-- =========================================================================================	
/*
create procedure tableMatch for full-text search
*/

delimiter $$

create PROCEDURE tableMatch(
	tableName	VARCHAR(50),
	keyWord		VARCHAR(200)
)

BEGIN

set @searchFull = CONCAT_ws(" ",'SELECT T.thePrecision, T.routineName, T.matrixType, T.structureType, T.url, P.problem, T.description FROM', tableName, 'AS T JOIN Problem AS P WHERE T.problem = P.id_p AND MATCH (routineName, matrixType, url, description) AGAINST("', keyWord, '"IN BOOLEAN MODE)'); 

PREPARE stmtFull FROM @searchFull;
EXECUTE stmtFull;
DEALLOCATE PREPARE stmtFull;

END$$

delimiter ;

-- =========================================================================================	

delimiter $$

CREATE PROCEDURE mySearch(
	keyWord		VARCHAR(200)
)

BEGIN

	IF keyWord = 'single' THEN
		call tableProc("Linear_Equation", "T.thePrecision = 's'");
		call tableProc("Linear_Least_Squares", "T.thePrecision = 's'");
		call tableProc("Sym_Eigen", "T.thePrecision = 's'");
		call tableProc("nonSym_Eigen", "T.thePrecision = 's'");
		call tableProc("SVD", "T.thePrecision = 's'");

	ELSEIF keyWord = 'double' THEN
		call tableProc("Linear_Equation", "T.thePrecision = 'd'");
		call tableProc("Linear_Least_Squares", "T.thePrecision = 'd'");
		call tableProc("Sym_Eigen", "T.thePrecision = 'd'");
		call tableProc("nonSym_Eigen", "T.thePrecision = 'd'");
		call tableProc("SVD", "T.thePrecision = 'd'");


	ELSEIF keyWord = 'complex' THEN
		call tableProc("Linear_Equation", "T.thePrecision = 'c'");
		call tableProc("Linear_Least_Squares", "T.thePrecision = 'c'");
		call tableProc("Sym_Eigen", "T.thePrecision = 'c'");
		call tableProc("nonSym_Eigen", "T.thePrecision = 'c'");
		call tableProc("SVD", "T.thePrecision = 'c'");


	ELSEIF keyWord = 'complex 16' OR keyWord = 'double complex' THEN
		call tableProc("Linear_Equation", "T.thePrecision = 'z'");
		call tableProc("Linear_Least_Squares", "T.thePrecision = 'z'");
		call tableProc("Sym_Eigen", "T.thePrecision = 'z'");
		call tableProc("nonSym_Eigen", "T.thePrecision = 'z'");
		call tableProc("SVD", "T.thePrecision = 'z'");


	ELSEIF keyWord like '%full%' THEN
		call tableProc("Linear_Equation", "T.structureType = 'f'");
		call tableProc("Linear_Least_Squares", "T.structureType = 'f'");
		call tableProc("Sym_Eigen", "T.structureType = 'f'");
		call tableProc("nonSym_Eigen", "T.structureType = 'f'");
		call tableProc("SVD", "T.structureType = 'f'");


	ELSEIF keyWord like '%band%' THEN
		call tableProc("Linear_Equation", "T.structureType = 'b'");
		call tableProc("Sym_Eigen", "T.structureType = 'b'");


	ELSEIF keyWord like '%pack%' THEN
		call tableProc("Linear_Equation", "T.structureType = 'p'");
		call tableProc("Sym_Eigen", "T.structureType = 'p'");


	ELSEIF keyWord like '%tridiag%' THEN
		call tableProc("Linear_Equation", "T.structureType = 't'");
		call tableProc("Sym_Eigen", "T.structureType = 't'");


	ELSE
		call tableMatch("Linear_Equation", keyWord);
		call tableMatch("Linear_Least_Squares", keyWord);
		call tableMatch("Sym_Eigen", keyWord);
		call tableMatch("nonSym_Eigen", keyWord);
		call tableMatch("SVD", keyWord);


	END IF;

END$$

delimiter ;
