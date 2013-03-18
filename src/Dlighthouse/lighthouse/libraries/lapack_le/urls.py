from django.conf.urls.defaults import *
from django.conf import settings


urlpatterns = patterns('lighthouse.views.lapack_le',
    url(r'^$', "search_forms", name="lapack_le"),
    url(r'^guided/problem/$', "guidedSearch_problem", name="lapack_le_problem"), 
    url(r'^guided/problem_equation/$', "guidedSearch_equation"),
    url(r'^guided/problem_equation_factor/$', "guidedSearch_factor", name="lapack_le_factor"),
    url(r'^guided/problem_equation_factor_complex$', "guidedSearch_complex"),
    url(r'^guided/problem_complex/$', "guidedSearch_complex"),
    url(r'^guided/problem_complex_matrixtype/$', "guidedSearch_matrixtype"),   
    url(r'^guided/problem_complex_matrixtype_storage/$', "guidedSearch_storage"),
    url(r'^guided/problem_complex_matrixtype_storage_precision/$', "guidedSearch_precision"),

    url(r'^advanced/form/$', "advancedForm"),
    url(r'^advanced/result/$', "advancedResult"),
    
    url(r'^update_session/$', "update_session"),
    url(r'^clear_session/$', "clear_session"),    
    url(r'^load_template/$', "load_template"),
    
    url(r'^keyword/$', "keywordResult"),
    url(r'^download/$', "downloadTemplate"),

    url(r'^petsc/linear_system/$', "petsc"),
    url(r'^petsc/linear_system/code/$', "petsc_code"),


    #(r'^script/$', runScript),
)
