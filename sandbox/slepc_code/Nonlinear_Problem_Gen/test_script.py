import os
import string 

nlevp = [
	'acoustic_wave_1d',
	'acoustic_wave_2d',
	'butterfly',
	'damped_beam',
	'pdde_stability',
	'planar_waveguide',
	'sleeper',
	'spring'
]

solvers = [
	'-pep_type toar',
	'-pep_type linear',
	'-pep_type qarnoldi -st_transform true'
]

bases = [
	'-pep_basis monomial',
	'-pep_basis chebyshev1',
	'-pep_basis chebyshev2',
	'-pep_basis legendre',
	'-pep_basis laguerre',
	'-pep_basis hermite'
]

extractions = [
	'-pep_extract none',
	'-pep_extract norm',
	'-pep_extract residual',#Does not work for linear and qarnoldi
	'-pep_extract structured'#Does not work for qarnoldi
]	

scales = [
	'-pep_scale none',
	'-pep_scale scalar',
	'-pep_scale diagonal',
	'-pep_scale both'
]	

spect_transforms = [
	'-st_type shell',
	'-st_type shift',
	'-st_type sinvert',
	'-st_type cayley',
	'-st_type precond'
]

magnitudes = [
	'-pep_largest_magnitude',
	'-pep_smallest_magnitude',
	'-pep_largest_real',
	'-pep_smallest_real',
	'-pep_largest_imaginary',
	'-pep_smallest_imaginary'
]