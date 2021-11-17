from multiprocessing import Process
from client import worker
from distributor import dealer
import zmq

context=zmq.Context()
def count(msg):
    pass
w=worker("localhost", ctxt=context, work=count)
d=dealer(ctxt=context)

def run_worker(w):
    w.run()

def run_dealer(d):
    d.run()

pworker=Process(target=run_worker, args=(w,))
pdealer=Process(target=run_dealer, args=(d,))
pdealer.start()
pworker.start()
pdealer.join()
pworker.join()
with open("binaries.txt","r") as f:
    lines=f.readlines()
    assert(len(lines)!=0)
