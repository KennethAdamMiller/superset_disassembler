from distributor import dealer
import zmq
import sys

if len(sys.argv) > 1:
    tests=int(sys.argv[1])

print("creating context")
context=zmq.Context()
print("created context")
b=dealer(ctxt=context, test_size=sys.argv[1])
b.run()
