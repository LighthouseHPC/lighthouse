from django.conf.urls.defaults import *
from django.conf import settings

### --- when using generic views --- ###
from django.views.generic import ListView, DetailView
from blog.models import Post

from django.contrib.syndication.views import Feed

class BlogFeed(Feed):
    title = "MySite -- using generic views"
    description = "Some ramblings of mine"
    link = "/blog/feed"
    
    def items(self):
        return Post.objects.all().order_by("-created")[:2]
    def item_title(self, item):
        return item.title
    def item_description(self, item):
        return item.body    
    def item_link(self, item):
        return u"/blog/%d" % item.id




urlpatterns = patterns('blog.views',
### --- The following lines use blog.views --- ###
    url(r'^$', "main", name='blogindex'),
    url(r"^(\d+)/$", "post", name="post"),
    url(r"^add_comment/(\d+)/$", "add_comment"),
    url(r"^month/(\d+)/(\d+)/$", "month"),
    url(r"^delete_comment/(\d+)/$", "delete_comment"),
    url(r"^delete_comment/(\d+)/(\d+)/$", "delete_comment"),
    url(r"^search/$", "MySearchView"),
    
    
### --- The following lines use generic views --- ###
    #url(r'^$', ListView.as_view(
    #    queryset = Post.objects.all().order_by("-created")[:2],
    #    template_name = "blog.html")),
    #url(r'^(?P<pk>\d+)$', DetailView.as_view(
    #    model = Post,
    #    template_name = "post.html")),
    #url(r'^archives/$', ListView.as_view(
    #    queryset = Post.objects.all().order_by("-created"),
    #    template_name = "archives.html")),
    #url(r'^tag/(?P<tag>\w+)$', 'tagpage'),
    #url(r'^feed/$', BlogFeed()),
)