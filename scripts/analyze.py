from client import worker
import subprocess
import sys
import zmq

def run_bap(addr, msg):
    disasm="bap superset_disasm --heuristics=TrimLimitedClamped,Liveness --ground_truth_bin={} {}"
    disasm=disasm.format(msg, msg)
    print(disasm)
    disasm=disasm.split(" ")
    analysis=subprocess.Popen(disasm, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    analysis.communicate()
    transfer_cmd="bap send_cache {} --send_cache=tcp://" + addr + ":9996".format(msg)
    transfer_cmd=transfer_cmd.split(" ")
    transfer=subprocess.Popen(transfer_cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    transfer.communicate()

context=zmq.Context()
worker(sys.argv[1], ctxt=context, work=run_bap)
worker.run()
