from django.http import HttpResponse
from django.views.decorators.csrf import csrf_exempt


from codeGen.templates import BTOGenerator
from django.shortcuts import render_to_response, redirect, render
from django.template import RequestContext




def index(request):
    context = {
        'scriptCode': request.session['userScript'], 
        'scriptOutput': request.session['scriptOutput'],
    }

    return render_to_response(
        'lighthouse/btoscript/index.html', 
        context_instance = RequestContext(request, context)
     )


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


