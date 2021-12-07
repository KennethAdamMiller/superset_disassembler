from distributor import dealer
import zmq
import sys

if len(sys.argv) > 1:
    tests=int(sys.argv[1])
else:
    tests=99999

context=zmq.Context()
b=dealer(ctxt=context, test_size=sys.argv[1])
b.run()
