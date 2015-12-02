import os
import string
nlevp = [
	'acoustic_wave_1d',
	'acoustic_wave_2d',
	'bicycle',
	#'bilby',
	'butterfly',
	'cd_player',
	'closed_loop',
	#'concrete',
	'damped_beam',
	'dirac',
	'foundation',
	'gen_hyper2',
	'gen_tantipal2',
	'gen_tpal2',
	#'gun',
	'hospital',
	#'intersection',
	'metal_strip',
	#'mirror',
	#'mobile_manipulator',
	'omnicam1',
	'omnicam2',
	'power_plant',
	#'qep1',
	'qep2',
	#'qep3',
	#'qep4',
	#'qep5',
	#'railtrack',
	#'railtrack2',
	#'relative_pose_5pt',
	#'relative_pose_6pt',
	'schrodinger',
	#'shaft',
	'sign1',
	'sign2',
	'sleeper',
	'speaker_box',
	'spring',
	#'spring_dashpot',
	#'surveillance',
	'wing',
	'wiresaw1',
	'wiresaw2'
]

solvers = [
	'-pep_type toar',
	'-pep_type linear',
	'-pep_type qarnoldi -st_transform true'
]
	

for qep in nlevp:
	for solver in solvers:
		mat_file = 'QEP_COEFFS/{0}/{0}'.format(qep)
		cmd = './qep -M {0}_m.petsc -K {0}_k.petsc -C {0}_d.petsc {1} >> test/{2}_test.txt'.format(mat_file,solver,qep)
		#print(cmd)
		os.system(cmd)
		
