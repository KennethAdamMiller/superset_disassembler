import zmq
import socket
import os

class worker:
    def skip(addr, msg):
        pass
    def __init__(self, addr, ctxt=None, work=skip):
        self.addr=addr
        self.work=work
        self.processed=set()
        if ctxt is None:
            self.context = zmq.Context()
        else:
            self.context = ctxt
    def run(self):
        worker = self.context.socket(zmq.REQ)
        worker.connect("tcp://" + self.addr + ":9999")
        results=self.context.socket(zmq.PUSH)
        results.connect("tcp://" + self.addr + ":9998")
        killed=self.context.socket(zmq.SUB)
        killed.connect("tcp://" + self.addr + ":9997")
        killed.setsockopt(zmq.SUBSCRIBE, b"exit")
        poller=zmq.Poller()
        poller.register(worker, zmq.POLLIN)
        poller.register(killed, zmq.POLLIN)
        worker.send(b"request work")
        do_work=True
        while do_work:
            socks = dict(poller.poll(30*1000))
            if worker in socks and socks[worker] == zmq.POLLIN:
                msg=worker.recv()
                print("worker recvd {}".format(msg), flush=True)
                if msg is not None and msg!=b"":
                    msg = msg.decode("utf-8")
                    self.work(self.addr, msg)
                    self.processed.add(msg)
                    results.send(str.encode(socket.gethostname() + ":" + msg))
                if msg==b"":
                    print("worker told to wait")
                worker.send(b"request work")
            elif killed in socks and socks[killed] == zmq.POLLIN:
                print("killed recvd msg", flush=True)
                killed.recv()
                do_work=False
                break
            else:
                break
        print("Worker exiting", flush=True)
