#!/bin/bash
echo "=====$0====="
eval $(opam config env)
source scripts/read_cache.sh
export features="TrimLimitedClamped,FixpointGrammar,Liveness"
export rounds=3
export gt_bin="${HOME}/workspace/x86-binaries/elf/findutils/gcc_findutils_32_O0_bigram"
args="${gt_bin} --ground_truth_bin=${gt_bin} --threshold=0.20 "
if [[ ! (-z ${phases}) ]]; then
    args+=" --invariants=${phases}"
fi
if [[ ! (-z ${analyses}) ]]; then
    args+=" --analyses=${analyses}"
fi
if [[ ! (-z ${features}) ]]; then
    args+=" --features=${features}"
fi
args+="--rounds=${rounds} "
echo "time bap superset_disasm --u ${args}"
time bap superset_disasm --u ${args}

fns=$(bap superset_distribution --metrics "%1","superset-disasm-metrics:false_negatives" ${args})
if [ 0 -eq ${fns} ]; then
    echo "Should have some false negatives";
    exit -1;
fi
