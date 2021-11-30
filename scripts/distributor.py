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
    def run(self):
        service = self.context.socket(zmq.REP)
        collector = self.context.socket(zmq.PULL)
        killed = self.context.socket(zmq.REQ)
        service.bind("tcp://*:9999")
        collector.bind("tcp://*:9998")
        killed.bind("tcp://*:9997")
        with open("./binaries.txt","r") as f:
            bins=f.readlines()
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
            transfer_cmd="bap recv_cache --perpetuate --bind_addr=tcp://*:9996"
            transfer_cmd=transfer_cmd.split(" ")
            #transfer=subprocess.Popen(transfer_cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            do_work=True
            worker_timeout=45*60
            print(len(bins))
            while do_work and (len(bins)!=0):
                socks = dict(poller.poll())
                print("msg received!")
                if service in socks and socks[service] == zmq.POLLIN:
                    msg = service.recv()
                    print(msg)
                    if msg==b"request work" and do_work:
                        s=bins.pop()
                        service.send(str.encode(s))
                        sent.appendleft((s,time.time()))
                        print("Sent {} tasks".format(len(sent)))
                    if msg==b"progress report" and do_work:
                        report="Processed {}, {} remaining.\n{}".format(len(results), len(bins), bins)
                        service.send(report)
                    if msg==b"exit":
                        service.send(b"exit received")
                        do_work=False
                    while len(sent) > 0:
                        (s,t) = sent.pop()
                        elapsed = time.time() - t
                        if elapsed > worker_timeout:
                            bins.append(s)
                        else:
                            sent.appendleft((s,t))
                            break
                #collect - upon reciept of a branch and commit, keep broadcast
                #a request for every file name to be recovered from cache
                #until all have been fulfilled
                if collector in socks and socks[collector] == zmq.POLLIN:
                    c=collector.recv()
                    results.add(c)
                    print("Recvd {}, {} total".format(c,len(results)))
        #transfer.communicate() #TODO
        print("broker exiting")
        killed.send(b"")
        killed.recv()
