#from malamute import MalamuteClient
import zmq
from collections import deque
import os
#cli arguments: addr

class dealer:
    def __init__(self, ctxt=None):
        if ctxt is None:
            self.context = zmq.Context()
        else:
            self.context = ctxt
    def run(self):
        #channels: command, collect, controller, dealer
        #service = MalamuteClient()
        
        service = self.context.socket(zmq.REP)
        collector = self.context.socket(zmq.PULL)
        killed = self.context.socket(zmq.REQ)
        #service.connect(addr, 100, b'distribution')
        service.bind("tcp://*:9999")
        collector.bind("tcp://*:9998")
        killed.bind("tcp://*:9997")
        #service.set_worker(b'command', b'binaries')
        #service.set_worker(b'collect', b'cache')
        #service.set_worker(b'controller', b'command')
        #service.set_worker(b'dealer', b'workqueue')
        with open("binaries.txt","r") as f:
            lines=f.readlines()
            lines=[s.strip() for s in lines]
            lines.sort(key=lambda f: os.stat(f).st_size, reverse=True)
            bins=deque(lines)
            sent=set()
            results=set()
            poller=zmq.Poller()
            poller.register(service, zmq.POLLIN)
            poller.register(collector, zmq.POLLIN)
            do_work=True
            while do_work and (len(bins)!=0):
                #cmd,sender,subject,content = msg
                #[subject, content] = msg
                socks = dict(poller.poll())
                if service in socks and socks[service] == zmq.POLLIN:
                    msg = service.recv()
                    if msg==b"request work" and do_work:
                        s=bins.pop()
                        service.send(str.encode(s))
                        sent.add(s)
                        print("Sent {} tasks".format(len(sent)))
                        #dealer - provide file names to be processed to workers
                        #service.sendfor(b"dealer", b"workqueue", None, 100, [str.encode(bpath)])
                    if msg==b"exit":
                        do_work=False
                #collect - upon reciept of a branch and commit, keep broadcast
                #a request for every file name to be recovered from cache
                #until all have been fulfilled
                if collector in socks and socks[collector] == zmq.POLLIN:
                    c=collector.recv()
                    results.add(c)
                    print("Recvd {} results".format(len(results)))
        killed.send(b"")
        killed.recv()
