from distributor import dealer
import zmq

context=zmq.Context()
b=dealer(ctxt=context)
b.run()
