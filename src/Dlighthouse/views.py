from django.contrib.auth import logout
from django.views.decorators.csrf import csrf_exempt
from django.http import HttpResponseRedirect
from django.shortcuts import render_to_response

#----for registration----#
#from django import forms
from django.contrib.auth.views import *
from django.contrib.auth.forms import UserCreationForm
from django.contrib.auth.models import User




@csrf_exempt
def logout_page(request):
    logout(request)
    return HttpResponseRedirect('/')
    

@csrf_exempt
def registration(request):
    if request.method == 'POST':
        form = UserCreationForm(request.POST)
        if form.is_valid():
            new_user = form.save()
            return HttpResponseRedirect("/")
        else:
            form = UserCreationForm()
            return render_to_response("registration/registration.html", {'form': form,})
    else:
        form = UserCreationForm()
        return render_to_response("registration/registration.html", {'form': form,})
    
