import zmq
from collections import deque
import os
import time

class dealer:
    def skip(self):
        pass
    def __init__(self, test_size, ctxt=None, work=skip):
        self.test_size=int(test_size)
        self.work=work
        if ctxt is None:
            self.context = zmq.Context()
        else:
            self.context = ctxt
        print("initialized", flush=True)
    def run(self):
        service = self.context.socket(zmq.REP)
        collector = self.context.socket(zmq.PULL)
        killed = self.context.socket(zmq.PUB)
        update = self.context.socket(zmq.PULL)
        service.bind("tcp://*:9999")
        collector.bind("tcp://*:9998")
        print("Binding kill socket", flush=True)
        killed.bind("tcp://*:9997")
        print("Entering service loop", flush=True)
        update.bind("tcp://*:9996")
        with open("./binaries.txt","r") as f:
            bins=f.readlines()
            orig_count=len(bins)
            bins=[s.strip() for s in bins]
            bins.sort(key=lambda f: os.stat(f).st_size, reverse=True)
            reordered=deque(bins)
            bins=[]
            while len(reordered)>0 and len(bins) < self.test_size:
                if len(reordered) %2 == 0:
                    bins.append(reordered.pop())
                else:
                    bins.append(reordered.popleft())
            bins=deque(bins)
            sent=deque()
            results=set()
            poller=zmq.Poller()
            poller.register(service, zmq.POLLIN)
            poller.register(collector, zmq.POLLIN)
            do_work=True
            worker_timeout=45*60
            print("Bins: {}".format(len(bins)), flush=True)
            while do_work and ((len(bins)!=0) or len(results)>=self.test_size or len(results)>=orig_count):
                socks = dict(poller.poll(15*1000))
                if service in socks and socks[service] == zmq.POLLIN:
                    msg = service.recv()
                    print(msg, flush=True)
                    if msg==b"request work" and do_work and len(bins) > 0:
                        s=bins.pop()
                        service.send(str.encode(s))
                        sent.appendleft((s,time.time()))
                        print("Sent {} tasks".format(len(sent)), flush=True)
                    elif len(bins)==0:
                        print("putting worker to sleep", flush=True)
                        service.send(b"")
                    if msg==b"progress report" and do_work:
                        report="Processed {}, {} remaining.\n{}".format(len(results), len(bins), bins)
                        service.send(report)
                    if msg==b"exit":
                        service.send(b"exit received")
                        do_work=False
                _sent=deque()
                while len(sent) > 0:
                    (s,t) = sent.pop()
                    elapsed = time.time() - t
                    if elapsed > worker_timeout:
                        bins.append(s)
                    else:
                        _sent.appendleft((s,t))
                        break
                sent=_sent
                #collect - upon reciept of a branch and commit, keep broadcast
                #a request for every file name to be recovered from cache
                #until all have been fulfilled
                if collector in socks and socks[collector] == zmq.POLLIN:
                    c=collector.recv()
                    results.add(c.split(":")[1])
                    print("Recvd {}, {} total".format(c,len(results)), flush=True)
                else:
                    print("do_work {}, len(bins)={}, len(results)={}".format(
                        do_work, len(bins), len(results)), flush=True)
        print("broker exiting", flush=True)
        killed.send(b"exit")
