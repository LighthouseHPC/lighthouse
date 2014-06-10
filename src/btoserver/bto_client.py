# This file represents the basic setup for a client making
# a single request to a running BTO Server.

from BTO import BTO_Client

HOST = 'localhost'
PORT = 9999
USER = 'salin'
OPTIONS = '-e'
FILE = 'dgemv.m'

client = BTO_Client()
client.submit_request(HOST, PORT, USER, OPTIONS, FILE)
