import string, types, sys, os, StringIO, re, shlex, json, zipfile

from django.contrib.auth.decorators import login_required
from django.core.servers.basehttp import FileWrapper
from django.http import HttpResponse
from django.shortcuts import render_to_response, redirect, render
from django.template import RequestContext
from django.template.loader import render_to_string
from django.views.decorators.csrf import csrf_exempt
from lighthouse.forms.petsc_le import *

	

###---------------- Petsc ------------------###
def petsc(request):    
	
  	context = {
  		'form': petsc_problem_form(),
  	}
	return render_to_response(
		'lighthouse/petsc_le/index.html', 
		context_instance=RequestContext(request, context)
	)

def linear_system(request):
	
	if request.method == 'POST':
		op = request.POST['operations']

		if op == 'Solve a system of linear equation':
			context = {
		  		'form': solve_linear_system_form(),
		  	}
		else:
			context = {
		  		'message': "Sorry, Lighthouse cannot generate PETSc code for eigenvalue problems yet.",
		  	}

		return render_to_response(
			'lighthouse/petsc_le/linear_system.html', 
			context_instance=RequestContext(request, context)
		)

def petsc_code(request):

	if request.method == 'POST':
		
		success = generateCode(request)
		
		if success:
			error = "no"
			message = "Awesome! You can download your PETSc program now."
			code = getCode()
			makefile = getMakefile()
			command = getCommands()

			context = {
	  			'form': solve_linear_system_form(),
	  			'error': error,
	  			'message': message,
			  	'code': code,
				'makefile': makefile,
				'command': command,
		  	}
		else:			
			error = "yes"
			message = "Sorry, Lighthouse can't generate code for you right now because it's undergoing some very rigorous server maintenance."
			code = ""
			makefile = ""
			command = ""

	  		context = {
	  			'form': solve_linear_system_form(),
	  			'error': error,
	  			'message': message,
		  	}
		
		return render_to_response(
			'lighthouse/petsc_le/linear_system.html', 
			context_instance=RequestContext(request, context)
		)

def generateCode(request):

	upload_matrix = request.POST['upload_matrix']

	input_format = ""
	
	if upload_matrix == "Yes":	
		matrix_file = request.FILES['matrix_file']
		handle_uploaded_file(matrix_file)
	elif upload_matrix == "No":
		input_format = request.POST['input_format'] 
		row_count = request.POST['row_count']    
		column_count = request.POST['column_count']
		complex_number = request.POST['complex_number']
		nonzero_structure = request.POST['nonzero_structure']
		numerical_symmetry = request.POST['numerical_symmetry']

	solution_type = request.POST['solution_type']
	output_format = request.POST['output_format']

	base_path = './lighthouse/libraries/petsc_le/work_dir/'

	if upload_matrix == "No":
		if input_format == "PETSc binary format":		
			if output_format == "PETSc binary format":
				zf = zipfile.ZipFile( base_path + 'sample.zip', mode='w')
				zf.write( base_path + "linear_solver.c")
				zf.write( base_path + "makefile")
				zf.write( base_path + "readme.txt")
				zf.write( base_path + "command_line_options.txt")
				zf.close()	
				return True	
	
	if upload_matrix == "Yes":
			if output_format == "PETSc binary format":
				zf = zipfile.ZipFile( base_path + 'sample.zip', mode='w')
				zf.write( base_path + "linear_solver.c")
				zf.write( base_path + "makefile")
				zf.write( base_path + "readme.txt")
				zf.write( base_path + "command_line_options.txt")
				zf.close()	
				return True			

	return False

def downloadCode(request):

	filepath = './lighthouse/libraries/petsc_le/work_dir/sample.zip'

	wrapper = FileWrapper(open(filepath))
	response = HttpResponse(wrapper, content_type='application/octet-stream')
	response['Content-Disposition'] = 'attachment; filename=%s' % "sample.zip"
	return response

def getCode():

	filepath = './lighthouse/libraries/petsc_le/work_dir/linear_solver.c'
	code = ""
	
	if os.path.isfile(filepath):
	   	with open(filepath, 'r') as f:
			code = f.read()
	
	return code

def getMakefile():

	filepath = './lighthouse/libraries/petsc_le/work_dir/makefile'
	makefile = ""
	
	if os.path.isfile(filepath):
	   	with open(filepath, 'r') as f:
			makefile = f.read()		
	
	return makefile

def getCommands():

	filepath = './lighthouse/libraries/petsc_le/work_dir/command_line_options.txt'
	commands = ""
	
	if os.path.isfile(filepath):
	   	with open(filepath, 'r') as f:
			commands = f.read()
				
	return commands

def handle_uploaded_file(f):
    with open('./lighthouse/libraries/petsc_le/work_dir/'+'matrix_file', 'wb+') as destination:
        for chunk in f.chunks():
            destination.write(chunk)