from django.conf.urls.defaults import *
from django.conf import settings


urlpatterns = patterns('forum.views',
### --- The following lines use forum.views --- ###
    url(r"^$", "main", name="forumList"),
    url(r"^forum/(\d+)/$", "forum", name="forum"),
    url(r"^thread/(\d+)/$", "thread", name="thread"),
    
    url(r"^post/(new_thread|reply)/(\d+)/$", "post", name="forumPost"),
    url(r"^reply/(\d+)/$", "reply", name="reply"),
    url(r"^new_thread/(\d+)/$", "new_thread", name="new_thread"),
    
    url(r"^search/$", "MyForumSearchView", name="forumSearch"),
)