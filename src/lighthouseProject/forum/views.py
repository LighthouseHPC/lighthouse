from django.core.paginator import Paginator, InvalidPage, EmptyPage
from django.core.urlresolvers import reverse
from django.core.context_processors import csrf
from django.http import HttpResponse, HttpResponseRedirect
from django.shortcuts import render_to_response
from lighthouseProject.settings import MEDIA_ROOT, MEDIA_URL
from forum.models import Forum, Thread, Post, ThreadForm, PostForm

from haystack.views import SearchView
from haystack.query import SearchQuerySet
from haystack.forms import ModelSearchForm




def main(request):
    """Main listing."""
    forums = Forum.objects.all()
    d = {"forums": forums, "user": request.user, "searchForm": ModelSearchForm()}
    return render_to_response("forum/list.html", d)



def mk_paginator(request, items, num_items):
    """Create and return a paginator."""
    paginator = Paginator(items, num_items)
    try: page = int(request.GET.get("page", '1'))
    except ValueError: page = 1

    try:
        items = paginator.page(page)
    except (InvalidPage, EmptyPage):
        items = paginator.page(paginator.num_pages)
    return items



def add_csrf(request, ** kwargs):
    d = dict(user=request.user, ** kwargs)
    d.update(csrf(request))
    return d


def forum(request, pk):
    """Listing of threads in a forum."""
    threads = Thread.objects.filter(forum=pk).order_by("-created")
    threads = mk_paginator(request, threads, 20)
    return render_to_response("forum/forum.html", add_csrf(request, threads=threads, pk=pk, searchForm= ModelSearchForm()))


def thread(request, pk):
    """Listing of posts in a thread. The first post is always the thread post."""
    thread = Post.objects.filter(thread=pk).order_by("created")[0]
    posts = Post.objects.filter(thread=pk).order_by("created")[1:]
    posts = mk_paginator(request, posts, 15)
    title = Thread.objects.get(pk=pk).title
    
    """creates the paginator with 2 items per page."""
    #paginator = Paginator(posts, 3)
    #
    #try: page = int(request.GET.get("page", '1'))
    #except ValueError: page = 1
    #
    #try:
    #    posts = paginator.page(page)
    #except (InvalidPage, EmptyPage):
    #    posts = paginator.page(paginator.num_pages)
        
    return render_to_response("forum/thread.html", add_csrf(request, thread=thread, posts=posts, pk=pk,
        title=title, searchForm= ModelSearchForm()))


def post(request, ptype, pk):
    """Display a post form."""
    action = reverse("%s" % ptype, args=[pk])
    if ptype == "new_thread":
        title = "Start New Topic"
        subject = ''
        form = ThreadForm()
    elif ptype == "reply":
        title = "Reply to " + "\""+ Thread.objects.get(pk=pk).title + "\""
        subject = "Re: " + Thread.objects.get(pk=pk).title
        form = PostForm(initial={'title': subject})
    return render_to_response("forum/post.html", add_csrf(request, subject=subject,
        action=action, title=title, form=form, searchForm= ModelSearchForm()))


def new_thread(request, pk):
    """Start a new thread."""
    if request.method == 'POST':
        form = ThreadForm(request.POST)
        if form.is_valid():
            p = form.cleaned_data
            forum = Forum.objects.get(pk=pk)
            """ save the thread in table Thread"""
            thread = Thread.objects.create(forum=forum, title=p["title"], creator=request.user)
            """ save the thread in table Post, too """
            Post.objects.create(thread=thread, title=p["title"], body=p["body"], creator=request.user)
        return HttpResponseRedirect(reverse("forum", args=[pk]))




def reply(request, pk):
    """Reply to a thread --> new post."""
    if request.method == 'POST':
        form = PostForm(request.POST)
        if form.is_valid():
            p = form.cleaned_data
            thread = Thread.objects.get(pk=pk)
            post = Post.objects.create(thread=thread, title=p["title"], body=p["body"], creator=request.user)
        return HttpResponseRedirect(reverse("thread", args=[pk]) + "?page=last")



""" perform haystack search """
def MyForumSearchView(request):
    form = ModelSearchForm(request.GET) # A form bound to the GET data
    if form.is_valid(): # All validation rules pass
        sqs = SearchQuerySet().models(Post)
        search_view = SearchView(template = "forum/forum_search.html", searchqueryset=sqs)
        return search_view(request)