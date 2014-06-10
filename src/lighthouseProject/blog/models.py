from django.db import models
#from taggit.managers import TaggableManager
from django.forms import ModelForm

""" to send email asynchronously (django_asynchronous_send_mail in python path) """
try:
    from django_asynchronous_send_mail import send_mail
except:
    from django.core.mail import send_mail


class Post(models.Model):
    title = models.CharField(max_length=100)
    body = models.TextField()
    created = models.DateTimeField(auto_now_add=True)
    #tags = TaggableManager()

    def __unicode__(self):
        return self.title



class Comment(models.Model):
    title = models.ForeignKey(Post)
    body = models.TextField()
    created = models.DateTimeField(auto_now_add=True)
    author = models.CharField(max_length=60)

    def __unicode__(self):
        return unicode("%s: %s" % (self.title, self.body[:60]))
    
    def save(self, *args, **kwargs):
        """Email when a comment is added."""
        if "notify" in kwargs and kwargs["notify"] == True:
            message = "Comment was was added to '%s' by '%s': \n\n%s" % (self.title, self.author, self.body)
            from_addr = "lighthouse@mcs.anl.gov"
            recipient_list = ["sasa1688@gmail.com"]
            send_mail( "New comment added", message, from_addr, recipient_list, fail_silently=False)
    
        if "notify" in kwargs:
            del kwargs["notify"]
    
        super(Comment, self).save(*args, ** kwargs)
        
    

class CommentForm(ModelForm):
    class Meta:
        model = Comment
        exclude = ["title"]
        
