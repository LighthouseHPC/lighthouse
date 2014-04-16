# Create your views here.
from django.http import HttpResponse
from django.shortcuts import render_to_response, redirect, render
from django.template import RequestContext
from lighthouse.forms.slepc_eprob import *
from lighthouse.models.slepc_eprob import *


def slepc_eprob(request):

    if request.method == 'POST':
        form = SlepcGuidedForm(request.POST)
        data = []
        message = form.errors#request.POST#
        #type = request.POST.get('type')
        if form.is_valid():
            data = form.cleaned_data
            message = form.cleaned_data
        request.session['form'] = form
        request.session['selectedRoutines'] = getSelectedRoutines(data)

    
    else:
        message = 'Checking'
        request.session['form'] = SlepcGuidedForm()
        request.session['selectedRoutines'] = []
    #remove later
    context = {
                'form'     : request.session['form'],
                'results'  : request.session['selectedRoutines'],
                'message'  : message
    }

    return render_to_response(
            'lighthouse/slepc_eprob/index.html', 
	    context_instance=RequestContext(request,context)
    )


