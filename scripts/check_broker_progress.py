import zmq
import sys

ctxt=zmq.Context()
sock=ctxt.socket(zmq.REQ)
sock.connect("tcp://" + sys.argv[1] + ":9998")
sock.send(b"progress report")
msg=sock.recv()
print(msg)
