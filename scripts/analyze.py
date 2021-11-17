from client import worker
def run_bap(msg):
    disasm="bap superset_disasm --heuristics=TrimLimitedClamped,Liveness --ground_truth_bin={} {}"
    disasm=disasm.format(msg, msg)
    #results.send(str.encode(disasm))
    print(disasm)
    disasm=disasm.split(" ")
    #analysis=subprocess.Popen(disasm, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    #analysis.communicate()

context=zmq.Context()
worker("broker", ctxt=context, work=run_bap)
worker.run()
