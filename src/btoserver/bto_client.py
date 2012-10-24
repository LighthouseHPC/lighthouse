from BTO import BTO_Client

HOST = 'localhost'
PORT = 9999
USER = 'salin'
OPTIONS = '-e'
FILE = 'dgemv.m'

client = BTO_Client()
client.submit_request(HOST, PORT, USER, OPTIONS, FILE)
