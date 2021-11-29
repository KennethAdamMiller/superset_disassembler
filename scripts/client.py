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
        killed=self.context.socket(zmq.REP)
        killed.connect("tcp://" + self.addr + ":9997")
        poller=zmq.Poller()
        poller.register(worker, zmq.POLLIN)
        poller.register(killed, zmq.POLLIN)
        worker.send(b"request work")
        do_work=True
        while do_work:
            socks = dict(poller.poll())
            if worker in socks and socks[worker] == zmq.POLLIN:
                msg=worker.recv()
                print("worker recvd {}".format(msg))
                if msg is not None:
                    msg = msg.decode("utf-8")
                    msg = os.path.expandvars(msg)
                    results.send(str.encode(socket.gethostname() + ":" + msg))
                    self.processed.add(msg)
                    self.work(self.addr, msg)
                worker.send(b"request work")
            if killed in socks and socks[killed] == zmq.POLLIN:
                print("killed recvd msg")
                killed.recv()
                killed.send(b"")
                do_work=False
                break
        print("Worker exiting")
