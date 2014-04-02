# Create your views here.
from django.http import HttpResponse
from django.shortcuts import render_to_response, redirect, render
from django.template import RequestContext

def slepc_eprob(request):

    return render_to_response(
            'lighthouse/slepc_eprob/index.html')#,
            #context_instance=RequestContext(request,context))


