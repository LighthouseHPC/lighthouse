from BTO import BTO_Client

HOST = 'localhost'
PORT = 9999
USER = 'dljohnso'
OPTIONS = '-e'
FILE = 'dgemv_row.m'

client = BTO_Client()
client.submit_request(HOST, PORT, USER, OPTIONS, FILE)
