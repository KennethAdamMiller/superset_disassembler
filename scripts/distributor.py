import zmq
from collections import deque
import os
import time

class dealer:
    def skip(self):
        pass

    def __init__(self, test_size, ctxt=None, work=skip, worker_timeout=45*60):
        self.test_size=int(test_size)
        self.work=work
        if ctxt is None:
            self.context = zmq.Context()
        else:
            self.context = ctxt
        self.resources={}
        self.outbound={}
        self.outbound_limit=4
        self.mem_adjustment=2000
        self.worker_timeout=worker_timeout
        self.sent=deque()
        self.results=set()
        self.orig_count=0
        self.size_map={}

    def scale_to_complexity(self, size):
        return 1024*size

    def scale_from_complexity(self, resource):
        return resource / 1024

    def above_outbound_limit(self, host):
        o = self.outbound.get(host)
        if o is not None:
            return len(o) >= self.outbound_limit
        return False

    def projection(self, hostname):
        r = self.resources.get(hostname)
        o = self.outbound.get(hostname)
        if r is not None and not self.above_outbound_limit(hostname):
            mem,cpus = r
            mem_outb=0
            if o is not None:
                mem_outb = self.scale_to_complexity(sum([outb for _,outb in o]))
            if cpus > 1 and ((mem - mem_outb - self.mem_adjustment) > 0):
                return self.find_nearest(self.scale_from_complexity(mem - mem_outb))
        return None

    def find_nearest(self, search_key):
        if len(self.size_map)==0:
            return None
        if search_key in self.size_map:
            return self.pop_from_size_map(search_key),search_key
        else:
            min_key = min(self.size_map.keys(), key = lambda key: abs(key-search_key))
            if min_key in self.size_map:
                return self.pop_from_size_map(min_key),min_key
        return None

    def remove_from_outbound(self, fname):
        for k in self.outbound:
            for (name,size) in self.outbound[k]:
                l=[]
                if name!=fname:
                    l.append((name,size))

    def process_failure(self, fname, size):
        self.remove_from_outbound(fname)
        self.add_to_size_map(size,fname)

    def set_outbound(self, host, fname, size):
        self.sent.appendleft([fname,size,time.time()])
        cur=self.outbound.get(host)
        if cur is not None:
            self.outbound[host]=cur.prepend((fname,size))
        else:
            self.outbound.setdefault(host, [(fname,size)])

    def send_bin(self, service, host):
        proj=self.projection(host)
        if proj is not None:
            fname,size = proj
            self.set_outbound(host, fname, size)
            service.send(str.encode(fname))
        return proj

    def recv_results(self, collector):
        c=collector.recv().decode("utf-8")
        host,fname=c.split(":")
        self.results.add(fname)
        self.remove_from_outbound(fname)

    def remaining(self):
        total=0
        for k in self.size_map:
            total+=len(self.size_map[k])
        return total

    def add_to_size_map(self, size, fname):
        l=self.size_map.get(size)
        if l is not None:
            l.append(fname)
            self.size_map[size]=l
        else:
            self.size_map[size]=[fname]

    def pop_from_size_map(self, size):
        l=self.size_map.get(size)
        if l is not None:
            r = l.pop()
            if len(l)==0:
                del self.size_map[size]
            return r
    def remove_from_size_map(self, size, fname):
        l=self.size_map.get(size)
        if l is not None:
            l.remove(fname)
            self.size_map[size]=l
        if len(self.size_map[size])==0:
            del self.size_map[size]

    def append_work(self, bins):
        self.orig_count+=len(bins)
        sizes=[(os.stat(f).st_size / (1024*1024) +
                (os.stat(f).st_size % (1024*1024) > 0), f) for f in bins]
        for (size,fname) in sizes:
            self.add_to_size_map(size,fname)
            
    def load_from_file(self, bin_file="./binaries.txt"):
        with open(bin_file,"r") as f:
            bins=f.readlines()
            bins=[s.strip() for s in bins]
            self.append_work(bins)

    def update_resources(self, hostname, free_mem, free_cores):
        self.resources[hostname]=[free_mem,free_cores]
        if hostname in self.outbound:
            del self.outbound[hostname]

    def check_sent_timeout(self):
        _sent=deque()
        while len(self.sent) > 0:
            fname,size,t = self.sent.pop()
            elapsed = time.time() - t
            if elapsed > self.worker_timeout:
                self.process_failure(fname,size)
            else:
                _sent.appendleft([fname,size,t])
                break
        self.sent=_sent
            
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
        poller=zmq.Poller()
        poller.register(service, zmq.POLLIN)
        poller.register(collector, zmq.POLLIN)
        poller.register(update, zmq.POLLIN)
        do_work=True
        print("Bins: {}".format(self.remaining()), flush=True)
        while do_work and len(results)<min(self.test_size, orig_count):
            socks = dict(poller.poll(15*1000))
            if service in socks and socks[service] == zmq.POLLIN:
                msg = service.recv().decode("utf-8")
                print(msg, flush=True)
                if msg.startswith("request work") and do_work and self.remaining() > 0:
                    r,host=msg.split(":")
                    p=self.send_bin(service,host)
                    if p is not None:
                        print("Sent {} tasks".format(len(sent)), flush=True)
                    else:
                        print("putting worker to sleep", flush=True)
                        service.send(b"")
                elif self.remaining()==0:
                    print("putting worker to sleep", flush=True)
                    service.send(b"")
                if msg.startswith("append_work"):
                    appwork,worklist=msg.split(":")
                    self.append_work(worklist.split("\n"))
                if msg=="progress report" and do_work:
                    report="Processed {}, {} remaining.\n{}".format(
                        len(results), self.remaining(), self.size_map.values())
                    service.send(str.encode(report))
                if msg=="exit":
                    service.send(b"exit received")
                    do_work=False

            #collect - upon reciept of a branch and commit, keep broadcast
            #a request for every file name to be recovered from cache
            #until all have been fulfilled
            if collector in socks and socks[collector] == zmq.POLLIN:
                self.recv_results(collector)

            if update in socks and socks[update] == zmq.POLLIN:
                msg=update.recv().decode("utf-8")
                hostname,free_mem,free_cores=msg.split(",")
                self.update_resources(hostname, free_mem/1024, free_cores)

            self.check_sent_timeout()
            print("do_work {}, remaining()={}, len(results)={}".format(
                do_work, self.remaining(), len(results)), flush=True)
        print("broker exiting", flush=True)
        killed.send(b"exit")
