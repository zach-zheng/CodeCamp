"""定义learning_logs的URL模式"""

from django.urls import path
from . import views

urlpatterns = [
    #homepage
    path('', views.index, name='index')
]