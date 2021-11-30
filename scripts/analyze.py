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
    disasm=disasm.split(" ")
    analysis=subprocess.Popen(disasm, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    analysis.run()
    analysis.communicate()
    get_digest="bap superset_cache --show_cache_digest {}".format(msg).split(" ")
    get_digest=subprocess.Popen(get_digest, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    get_digest.run()
    digest,_=get_digest.communicate()
    transfer_cmd="bap send_cache {} --destination=tcp://" + addr + ":9996 --cache_digest={}".format(msg, digest)
    transfer_cmd=transfer_cmd.split(" ")
    transfer=subprocess.Popen(transfer_cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    transfer.run()
    transfer.communicate()

context=zmq.Context()
w=worker(sys.argv[1], ctxt=context, work=run_bap)
w.run()
