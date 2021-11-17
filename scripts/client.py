import zmq
import subprocess
#from malamute import MalamuteClient

#TODO setup the cli arguments: addr, corpora dirs,
class worker:
    def skip(arg):
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
        #worker = MalamuteClient()
        #worker.connect(addr, 100, b'dealer')
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
                if msg is not None:
                    msg = msg.decode("utf-8")
                    #results.send(str.encode(msg))
                    self.processed.add(msg)
                    self.work(msg)
                worker.send(b"request work")
            #[subject,content] = msg
            #receive shutdown command
            #if subject==b"command":
            if killed in socks and socks[killed] == zmq.POLLIN:
                killed.recv()
                killed.send(b"")
                do_work=False
                break
