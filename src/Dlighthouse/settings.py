# Django settings for Dlighthouse project.

import os
import socket



DEBUG = True
TEMPLATE_DEBUG = DEBUG


# setting branches for deploying Django to the production server
#if socket.gethostname() == 'cookie.mcs.anl.gov':
#    DEBUG = TEMPLATE_DEBUG = True
#else:
#    DEBUG = TEMPLATE_DEBUG = False




ADMINS = (
    # ('Your Name', 'your_email@example.com'),
)

MANAGERS = ADMINS

DATABASES = {
    'default': {
        'ENGINE': 'django.db.backends.mysql', # Add 'postgresql_psycopg2', 'postgresql', 'mysql', 'sqlite3' or 'oracle'.
        'NAME': 'shark',                      # Or path to database file if using sqlite3.
        'USER': 'root',                      # Not used with sqlite3.
        'PASSWORD': 'yellow1234',                  # Not used with sqlite3.
        'HOST': '127.0.0.1',                      # Set to empty string for localhost. Not used with sqlite3.
        'PORT': '3306',                      # Set to empty string for default. Not used with sqlite3.
        'OPTIONS': {
                "init_command": "SET storage_engine=MyISAM",
                } 
    }
}

# Local time zone for this installation. Choices can be found here:
# http://en.wikipedia.org/wiki/List_of_tz_zones_by_name
# although not all choices may be available on all operating systems.
# On Unix systems, a value of None will cause Django to use the same
# timezone as the operating system.
# If running in a Windows environment this must be set to the same as your
# system time zone.
TIME_ZONE = 'America/Chicago'

# Language code for this installation. All choices can be found here:
# http://www.i18nguy.com/unicode/language-identifiers.html
LANGUAGE_CODE = 'en-us'

SITE_ID = 1

# If you set this to False, Django will make some optimizations so as not
# to load the internationalization machinery.
USE_I18N = True

# If you set this to False, Django will not format dates, numbers and
# calendars according to the current locale
USE_L10N = True

SITE_ROOT = os.path.realpath(os.path.dirname(__file__)) 

TEMPLATE_DIRS = (
    # Put strings here, like "/home/html/django_templates" or "C:/www/django/templates".
    # Always use forward slashes, even on Windows.
    # Don't forget to use absolute paths, not relative paths.
	#'/disks/large/home/salin/Documents/Lighthouse/Dlighthouse/templates', -->This will cause problem when deplying.
        os.path.join(SITE_ROOT, 'templates'),
)

TEMPLATE_ROOT = os.path.join(SITE_ROOT, 'templates') 
TEMPLATE_URL = '/templates/'



# Absolute filesystem path to the directory that will hold user-uploaded files.
# Example: "/home/media/media.lawrence.com/media/"
MEDIA_ROOT = os.path.join(SITE_ROOT, 'media')

# URL that handles the media served from MEDIA_ROOT. Make sure to use a
# trailing slash.
# Examples: "http://media.lawrence.com/media/", "http://example.com/media/"
MEDIA_URL = '/media/'


# Absolute path to the directory static files should be collected to.
# Don't put anything in this directory yourself; store your static files
# in apps' "static/" subdirectories and in STATICFILES_DIRS.
# Example: "/home/media/media.lawrence.com/static/"
STATIC_ROOT = ''

# URL prefix for static files.
# Example: "http://media.lawrence.com/static/"
STATIC_URL = '/static/'

# URL prefix for admin static files -- CSS, JavaScript and images.
# Make sure to use a trailing slash.
# Examples: "http://foo.com/static/admin/", "/static/admin/".
ADMIN_MEDIA_PREFIX = '/static/admin/'

# Additional locations of static files
STATICFILES_DIRS = (
    # Put strings here, like "/home/html/static" or "C:/www/django/static".
    # Always use forward slashes, even on Windows.
    # Don't forget to use absolute paths, not relative paths.
    os.path.join(SITE_ROOT, 'static'),
)

# List of finder classes that know how to find static files in
# various locations.
STATICFILES_FINDERS = (
    'django.contrib.staticfiles.finders.FileSystemFinder',
    'django.contrib.staticfiles.finders.AppDirectoriesFinder',
#    'django.contrib.staticfiles.finders.DefaultStorageFinder',
    'dajaxice.finders.DajaxiceFinder',
)

# Make this unique, and don't share it with anybody.
SECRET_KEY = 'o9)d2r6#pt%l@d9^t8tvya+l1^i=pux3s$18i1m^0c^j1hx1rt'

LOGIN_URL = '/login/'

# List of callables that know how to import templates from various sources.
TEMPLATE_LOADERS = (
    'django.template.loaders.filesystem.Loader',
    'django.template.loaders.app_directories.Loader',
    'django.template.loaders.eggs.Loader',
)

MIDDLEWARE_CLASSES = (
    'django.middleware.common.CommonMiddleware',
    'django.contrib.sessions.middleware.SessionMiddleware',
    'django.middleware.csrf.CsrfViewMiddleware',
    'django.contrib.auth.middleware.AuthenticationMiddleware',
    'django.contrib.messages.middleware.MessageMiddleware',
    'dojango.middleware.DojoCollector',
    #'django_pdb.middleware.PdbMiddleware',
)

ROOT_URLCONF = 'Dlighthouse.urls'



# Haystack configuration.
HAYSTACK_CONNECTIONS = {
    'default': {
        'ENGINE': 'haystack.backends.whoosh_backend.WhooshEngine',
        'PATH': os.path.join(os.path.dirname(__file__), 'whoosh_index'),
    },
}



INSTALLED_APPS = (
    'django.contrib.auth',
    'django.contrib.humanize',
    'django.contrib.contenttypes',
    'django.contrib.sessions',
    #'django.contrib.sites',
    'django.contrib.messages',
    'django.contrib.staticfiles',
    # Uncomment the next line to enable the admin:
    'django.contrib.admin',
    # Uncomment the next line to enable admin documentation:
    # 'django.contrib.admindocs',
    'lighthouse',
    'blog',
    'haystack',
    'dojango',
    'taggit',
    
    #For dajax
    'dajaxice',
    'dajax',
    
    #For django-pdb
    #'django_pdb',
    
    #for django-registration
    'registration',
)




DOJANGO_DOJO_THEME = "claro"


# A sample logging configuration. The only tangible logging
# performed by this configuration is to send an email to
# the site admins on every HTTP 500 error.
# See http://docs.djangoproject.com/en/dev/topics/logging for
# more details on how to customize your logging configuration.
LOGGING = {
    'version': 1,
    'disable_existing_loggers': False,
    'handlers': {
        'mail_admins': {
            'level': 'ERROR',
            'class': 'django.utils.log.AdminEmailHandler'
        }
    },
    'loggers': {
        'django.request': {
            'handlers': ['mail_admins'],
            'level': 'ERROR',
            'propagate': True,
        },
    }
}

TEMPLATE_CONTEXT_PROCESSORS = (
  "django.contrib.auth.context_processors.auth",
  "django.core.context_processors.media",
  "django.core.context_processors.static",
  "django.core.context_processors.csrf",

# Must define a function "templates(request)" in django.core.context_processors 
# in order to enable the template tag {{ templates }} for TEMPLATE_URL
  #"django.core.context_processors.templates",

# For dajaxice.
  "django.core.context_processors.request",
  'django.contrib.messages.context_processors.messages'
)


# For dajaxice.
DAJAXICE_MEDIA_PREFIX = "dajaxice"
DAJAXICE_DEBUG = True
DAJAXICE_JS_DOCSTRINGS = True
#DAJAXICE_NOTIFY_EXCEPTIONS = True



##django-registration
##for settig up TEST email
#note: run 'python -m smtpd -n -c DebuggingServer localhost:1025' first
ACCOUNT_ACTIVATION_DAYS = 7
EMAIL_HOST = 'localhost'
EMAIL_PORT = 25
EMAIL_USE_TLS = False
DEFAULT_FROM_EMAIL = 'lighthouse@mcs.anl.gov'

##for email setup
#ACCOUNT_ACTIVATION_DAYS = 7
#EMAIL_HOST = 'smtp.gmail.com'
#EMAIL_PORT = 
#EMAIL_USE_TLS = True
#EMAIL_HOST_USER = '@mcs.anl.gov'
#EMAIL_HOST_PASSWORD = 'emails_password'
#DEFAULT_FROM_EMAIL = 'lighthouse@mcs.anl.gov'



import logging
#logging.basicConfig(level=logging.DEBUG)


