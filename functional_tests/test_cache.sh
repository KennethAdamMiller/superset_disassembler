#!/bin/bash
echo "====$0===="
eval $(opam config env)
source scripts/read_cache.sh
#export SUPERSET_FRONTEND=1
export heuristics="TrimLimitedClamped,FixpointGrammar,Liveness"
export rounds=3
export gt_bin="${HOME}/workspace/x86-binaries/elf/findutils/gcc_findutils_32_O0_bigram"
args="${gt_bin} --ground_truth_bin=${gt_bin} "
suff+=" --invariants=\"\""
suff+=" --analyses=\"\""
suff+=" --features=\"\""
echo "Executing for ground truth"
if [[ ! (-z ${phases}) ]]; then
    args+=" --invariants=${phases} "
fi
if [[ ! (-z ${analyses}) ]]; then
    args+=" --analyses=${analyses} "
fi
if [[ ! (-z ${heuristics}) ]]; then
    args+=" --heuristics=${heuristics} "
fi
args+="--rounds=${rounds} "
bap superset_cache --reset_cache ${gt_bin}
echo "time bap superset_disasm --u ${args}"
time bap superset_disasm --u ${args}

read_cache ${args}
