from django.http import HttpResponse
from django.views.decorators.csrf import csrf_exempt
from django.shortcuts import render_to_response, redirect, render
from django.template import RequestContext

from lighthouse.codeGen.templates import BTOGenerator



def index(request):
    userSc = ''
    scrOut = ''

    try:
        userSc = request.session['userScript']
        scrOut = request.session['scriptOutput']
    except KeyError, e:
        pass

    context = {
        'scriptCode'  : userSc,
        'scriptOutput': scrOut,
    }

    return render_to_response(
        'lighthouse/btoscript/index.html', 
        context_instance = RequestContext(request, context)
     )


def args(request):
    return render_to_response('lighthouse/btoscript/args.txt')


###---------------- Script ------------------###

@csrf_exempt
def runScript(request):
    code = request.POST.get('scriptCode')

    if code == "":
        request.session['userScript'] = ""
        request.session['scriptOutput'] = ""
        output = ""
    else:
        bto = BTOGenerator()
        output = bto.generateCode(str(code))		
        request.session['userScript'] = code
        request.session['scriptOutput'] = output

    request.session.modified = True
    return HttpResponse(output)


