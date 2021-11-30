from distributor import dealer
import subprocess, shlex
import zmq
import sys

if len(sys.argv) > 1:
    tests=int(sys.argv[1])
else:
    tests=99999

print("creating context")
context=zmq.Context()
print("created context")

recv_cmd=shlex.split('bap recv_cache --perpetuate --bind_addr="tcp://*:9996"')
recvr=subprocess.Popen(recv_cmd, start_new_session=True)

b=dealer(ctxt=context, test_size=sys.argv[1])
b.run()
recvr.communicate()
