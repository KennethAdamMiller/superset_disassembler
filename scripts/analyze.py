from client import worker
import subprocess
import sys
import zmq
import os

def run_bap(addr, msg):
    disasm="bap superset_disasm --heuristics=TrimLimitedClamped,Liveness --ground_truth_bin={} {}"
    disasm=disasm.format(msg, msg)
    print(disasm,flush=True)
    disasm=disasm.split(" ")
    analysis=subprocess.Popen(disasm, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    output,errs=analysis.communicate()
    output=output.decode("utf-8")
    errs=errs.decode("utf-8")
    print(output,flush=True)
    print(errs,flush=True)

context=zmq.Context()
w=worker(sys.argv[1], ctxt=context, work=run_bap)
w.run()
