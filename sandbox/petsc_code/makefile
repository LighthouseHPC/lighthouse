include ${PETSC_DIR}/conf/variables
include ${PETSC_DIR}/conf/rules

lin_sol_par_real_batch: lin_sol_par_real_batch.o  chkopts
	-${CLINKER} -o lin_sol_par_real_batch lin_sol_par_real_batch.o  ${PETSC_KSP_LIB}
	${RM} lin_sol_par_real_batch.o

mat_conv_real: mat_conv_real.o  chkopts
	-${CLINKER} -o mat_conv_real mat_conv_real.o  ${PETSC_MAT_LIB}
	${RM} mat_conv_real.o

mat_props_real: mat_props_real.o  chkopts
	-${CLINKER} -o mat_props_real mat_props_real.o  ${PETSC_MAT_LIB}
	${RM} mat_props_real.o

mat_conv_comp: mat_conv_comp.o  chkopts
	-${CLINKER} -o mat_conv_comp mat_conv_comp.o  ${PETSC_MAT_LIB}
	${RM} mat_conv_comp.o

lin_sol_seq_real_batch: lin_sol_seq_real_batch.o  chkopts
	-${CLINKER} -o lin_sol_seq_real_batch lin_sol_seq_real_batch.o  ${PETSC_KSP_LIB}
	${RM} lin_sol_seq_real_batch.o

lin_sol_par_real: lin_sol_par_real.o  chkopts
	-${CLINKER} -o lin_sol_par_real lin_sol_par_real.o  ${PETSC_KSP_LIB}
	${RM} lin_sol_par_real.o

ex10: ex10.o  chkopts
	-${CLINKER} -o ex10 ex10.o  ${PETSC_KSP_LIB}
	${RM} ex10.o

ex11: ex11.o  chkopts
	-${CLINKER} -o ex11 ex11.o  ${PETSC_KSP_LIB}
	${RM} ex11.o
