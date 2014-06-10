import datetime
from haystack import indexes
from forum.models import Post


""" Every SearchIndex requires there be one (and only one) field with document=True. """
""" This indicates to both Haystack and the search engine about which field is the primary field for searching within. """
""" RealTimeSearchIndex will automatically update the index when a Post and Comment are created or changed. """

class PostIndex(indexes.SearchIndex, indexes.Indexable):
    """ Note: the text field is enough to do a full-text search """
    """ The other fields are only for further filtering the search results. """
    text = indexes.CharField(document=True, use_template=True)
    title = indexes.CharField(model_attr='title')
    created = indexes.DateTimeField(model_attr='created')

    def get_model(self):
        return Post

    def index_queryset(self):
        """Used when the entire index for model is updated."""
        """A common theme is to allow admin users to add future content but have it not display on the site until that future date is reached."""
        """We specify a custom index_queryset method to prevent those future items from being indexed."""
        return self.get_model().objects.filter(created__lte=datetime.datetime.now())
    
    """ for desplaying the comments after search """
    """ use {{ result.comments }} in search.html """
    #comments = indexes.MultiValueField()
    #def prepare_comments(self, obj):
    #    return [a for a in obj.comment_set.all()]
    