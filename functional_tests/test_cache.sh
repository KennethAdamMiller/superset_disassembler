#!/bin/bash
echo "====$0===="
eval $(opam config env)
source scripts/read_cache.sh
#export SUPERSET_FRONTEND=1
export features="TrimLimitedClamped,FixpointGrammar,Liveness"
export rounds=3
export gt_bin="${HOME}/workspace/x86-binaries/elf/findutils/gcc_findutils_32_O0_bigram"
args="${gt_bin} --ground_truth_bin=${gt_bin} "
suff+="--metrics \"tps: %1, fns: %2\",\"superset-disasm-metrics:true_positives\",\"superset-disasm-metrics:false_negatives\""
echo "Executing for ground truth"
echo "time bap superset_distribution --u ${args} ${suff} "
time bap superset_distribution --u ${args} ${suff} 
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

read_cache ${args}
