from django.db import models
from django.contrib.auth.models import User
from django.contrib import admin
from django.forms import ModelForm
from string import join
from lighthouseProject.settings import MEDIA_ROOT

class Forum(models.Model):
    title = models.CharField(max_length=60)
    
    def __unicode__(self):
        return self.title
    
    def num_threads(self):
        return self.thread_set.count()
    
    def num_replies(self):
        return sum([t.num_posts() for t in self.thread_set.all()])-self.thread_set.count()

    def last_post(self):
        if self.thread_set.count():
            last = None
            for t in self.thread_set.all():
                l = t.last_post()
                if l:
                    if not last: last = l
                    elif l.created > last.created: last = l
            return last



class Thread(models.Model):
    title = models.CharField(max_length=60)
    created = models.DateTimeField(auto_now_add=True)
    creator = models.ForeignKey(User, blank=True, null=True)
    body = models.TextField(max_length=10000)
    forum = models.ForeignKey(Forum)

    def __unicode__(self):
        return unicode(self.creator) + " - " + self.title
    
    def num_posts(self):
        return self.post_set.count()

    def num_replies(self):
        return self.post_set.count() - 1

    def last_post(self):
        if self.post_set.count():
            return self.post_set.order_by("created")[0]
        
        

class Post(models.Model):
    title = models.CharField(max_length=60)
    created = models.DateTimeField(auto_now_add=True)
    creator = models.ForeignKey(User, blank=True, null=True)
    thread = models.ForeignKey(Thread)
    body = models.TextField(max_length=10000)

    def __unicode__(self):
        return u"%s - %s - %s" % (self.creator, self.thread, self.title)

    def short(self):
        return u"%s - %s" % (self.title, self.creator)
    
    def short_time(self):
        return u"%s" % (self.created.strftime("%b %d, %I:%M %p"))
    short.allow_tags = True
    
    
    
""" Forms """
    
class ThreadForm(ModelForm):
    class Meta:
        model = Thread
        exclude = ["forum", "creator"]
        
        
class PostForm(ModelForm):
    class Meta:
        model = Post
        exclude = ["thread", "creator"]
