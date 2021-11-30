from client import worker
import subprocess
import sys
import zmq
import os

def run_bap(addr, msg):
    disasm="bap superset_disasm --heuristics=TrimLimitedClamped,Liveness --ground_truth_bin={} {}"
    disasm=disasm.format(msg, msg)
    print(disasm)
    path=os.environ["PATH"]
    print(path)
    disasm=['/usr/bin/env', '-P', path] + disasm.split(" ")
    analysis=subprocess.Popen(disasm, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    analysis.communicate()
    transfer_cmd="bap send_cache {} --send_cache=tcp://" + addr + ":9996".format(msg)
    transfer_cmd=transfer_cmd.split(" ")
    transfer=subprocess.Popen(transfer_cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    transfer.communicate()

context=zmq.Context()
w=worker(sys.argv[1], ctxt=context, work=run_bap)
w.run()
