from django.conf.urls.defaults import *
from django.conf import settings

from Driver.views import *


urlpatterns = patterns('',
### --- The following lines use Driver.views --- ###
    (r'^$', search_forms),
    (r'^guided/problem/$', guidedSearch_problem), 
    (r'^guided/problem_equation/$', guidedSearch_equation),
    (r'^guided/problem_equation_factor/$', guidedSearch_factor),
    (r'^guided/problem_equation_factor_complex$', guidedSearch_complex),
    (r'^guided/problem_complex/$', guidedSearch_complex),
    (r'^guided/problem_complex_matrixtype/$', guidedSearch_matrixtype),   
    (r'^guided/problem_complex_matrixtype_storage/$', guidedSearch_storage),
    (r'^guided/problem_complex_matrixtype_storage_precision/$', guidedSearch_precision),

    (r'^advanced/form/$', advancedForm),
    (r'^advanced/result/$', advancedResult),
    
    (r'^update_session/$', update_session),
    (r'^clear_session/$', clear_session),    
    (r'^load_template/$', load_template),    
    
    (r'^keyword/$', keywordResult),
    (r'^download/$', downloadTemplate),
    (r'^script/$', runScript),
)
