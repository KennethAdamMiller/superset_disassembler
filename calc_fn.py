import sys

def file_to_list(fname):
    try:
        with open(fname) as f:
            return [x for x in f.read().splitlines()]
    except Exception: return []

def addr_of_str(x):
    try:
        if ":" in x:
            x=x.split(":")[0]
        return hex(int(x.strip().lower(),16))
    except Exception: return ""

true_positives  = set(map(addr_of_str, file_to_list(sys.argv[1])))

objdump_results = set(map(addr_of_str, file_to_list(sys.argv[2])))
pgm_results     = set(map(addr_of_str, file_to_list(sys.argv[3])))

print("Number of true positives: ", len(true_positives))
print("Number of objdump addrs: ", len(objdump_results))
print("Number of pgm addrs: ", len(pgm_results))
print("Objdump false negatives: ", len(true_positives.difference(objdump_results)))
print("pgm false negatives: ", len(true_positives.difference(pgm_results)))
