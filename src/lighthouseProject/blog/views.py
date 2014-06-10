from django.shortcuts import render_to_response, redirect, render
from django.http import HttpResponse, HttpResponseRedirect
from django.template import RequestContext
from django.contrib.auth.models import User
from django.contrib.contenttypes.models import ContentType
from django.core.paginator import Paginator, InvalidPage, EmptyPage
from django.core.urlresolvers import reverse
from django.core.context_processors import csrf
from django.views.decorators.csrf import csrf_exempt
from django import forms
from django.forms import ModelForm

from haystack.views import SearchView 
from haystack.query import SearchQuerySet
from haystack.forms import ModelSearchForm

from calendar import month_name
import time

from blog.models import Post, Comment, CommentForm




def main(request):
    """Main listing."""
    """The list of posts is ordered by created time in reverse order."""
    posts = Post.objects.all().order_by("-created")
    
    """creates the paginator with 2 items per page."""
    paginator = Paginator(posts, 3)

    try: page = int(request.GET.get("page", '1'))
    except ValueError: page = 1

    try:
        posts = paginator.page(page)
    except (InvalidPage, EmptyPage):
        posts = paginator.page(paginator.num_pages)

    d = {"posts": posts, "user": request.user, "months": mkmonth_lst(), "searchForm": ModelSearchForm()}
    return render_to_response("blog/list.html", d)



def post(request, pk):
    """Single post with comments and a comment form."""
    """post below gets the title of the post. note: Post.objects.get()"""
    post = Post.objects.get(pk=int(pk))
    comments = Comment.objects.filter(title=post)
    d = {"post":post, "user":request.user, "months": mkmonth_lst(), "comments": comments, "form": CommentForm(), "searchForm": ModelSearchForm()}
    d.update(csrf(request))
    return render_to_response("blog/post.html", d)





@csrf_exempt
def add_comment(request, pk):
    p = request.POST
    #print p
    """output: <QueryDict: {u'body': [u'good eats'], u'csrfmiddlewaretoken': [u'F4ioZcjG9UQQlYzBVQWKAnhOdRavY2Wa'], u'author': [u'salin']}>"""

    if p.has_key("body") and p["body"]:
        author = "Anonymous"
        if p["author"]:
            author = p["author"]
        
        """ Create a form to edit an existing Comment """    
        comment = Comment(title=Post.objects.get(pk=pk))
        
        """ Create a form to edit an existing Comment, but use POST data to populate the form. """
        form_comment = CommentForm(p, instance=comment)
        
        form_comment.fields["author"].required = False
        
        """ Create, but don't save the new author instance. """
        comment = form_comment.save(commit=False)
    
        """ Modify the author in some way."""   
        comment.author = author
        
        """ Comment Notification. """ 
        notify = True
        if comment.author == "salin":
            notify = False
        
        """ Save the new instance."""
        comment.save(notify=notify)
        return HttpResponseRedirect(reverse("post", args=[pk]))
        #d = {'test': comment}
        #return render_to_response("blog/test.html", d)
    else:
        html ="<html><body>Invalid Form!</body></html>"
        return HttpResponse(html)
    
   
   
def mkmonth_lst():
    """Make a list of months to show archive links."""

    if not Post.objects.count():
        return []

    # set up vars
    year, month = time.localtime()[:2]
    first = Post.objects.order_by("created")[0]
    fyear = first.created.year
    fmonth = first.created.month
    months = []

    # loop over years and months
    for y in range(year, fyear-1, -1):
        start, end = 12, 0
        if y == year:
            start = month
        if y == fyear:
            end = fmonth-1

        for m in range(start, end, -1):
            months.append((y, m, month_name[m]))
    return months


    
    
def month(request, year, month):
    """Monthly archive."""
    posts = Post.objects.filter(created__year=year, created__month=month)
    d = {"posts": posts, "user": request.user, "months": mkmonth_lst(), "archive": True}
    return render_to_response("blog/list.html", d)




def delete_comment(request, post_pk, pk=None):
    """Delete comment(s) with primary key `pk` or with pks in POST."""
    if request.user.is_staff:
        if not pk:
            pklst = request.POST.getlist("delete")
        else:
            pklst = [pk]

        for pk in pklst:
            Comment.objects.get(pk=pk).delete()
        return HttpResponseRedirect(reverse("post", args=[post_pk]))
    

    


""" modify Haystack SearchView """
class MyHaystackSearchView(SearchView):
    def __call__(self, request, some_var):
        self.some_var = some_var
        return super(MyHaystackSearchView, self).__call__(request)
    
    def extra_context(self):
        return {
            "months": mkmonth_lst(),
            "searchForm": ModelSearchForm()
        }




def MySearchView(request):
    modelList = []
    if request.method == 'GET': # If the form has been submitted...
        form = ModelSearchForm(request.GET) # A form bound to the GET data
        if form.is_valid(): # All validation rules pass
            answer = form.cleaned_data['models']
            if answer == []:
                sqs = SearchQuerySet().models(Post, Comment)
            else:
                for model_name in answer:
                    # Turen a string into a class and create a list of model classes for sqs
                    ct = ContentType.objects.get(model=model_name.split(".")[1])
                    modelList.append(ct.model_class())
                sqs = SearchQuerySet().models(*modelList)
            search_view = MyHaystackSearchView(template = "blog/blog_search.html", searchqueryset=sqs)
            return search_view(request, "hello")

            


    
    
    

def tagpage(request, tag):
    posts = Post.objects.filter(tags__name=tag)
    d = {"posts": posts, "tag": tag}
    return render_to_response("tagpage.html", d)