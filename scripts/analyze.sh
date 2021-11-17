#!/bin/bash

compute_disasm() {
    gt_bin=$1
    invariants=$2
    analyses=$3
    heuristics=$4
    rounds=$5
    args="./$(basename ${gt_bin}) --ground_truth_bin=${gt_bin} --rounds=${rounds} "
    if [[ ! (-z ${invariants}) ]]; then
	args+=" --invariants=${invariants}"
    fi
    if [[ ! (-z ${analyses}) ]]; then
	args+=" --analyses=${analyses}"
    fi
    if [[ ! (-z ${heuristics}) ]]; then
	args+=" --heuristics=${heuristics}"
    fi
    echo "time bap superset_disasm ${args}"
    time bap superset_disasm ${args}
}
export -f compute_disasm

analyze() {
    export binary=${1}
    export heuristics=${2}
    echo "Usage: $0 <bin>=${binary} <heuristics>=${heuristics}"
    has_error=true
    while ${has_error}; do
	has_error=false
	echo "Processing ${workdir}/$(basename ${binary}) for ${heuristics}"
	cp "${binary}" ./
	strip "./$(basename ${binary})"
	compute_disasm "${binary}" "" "" "${heuristics}" 6
	rm -f "./$(basename ${binary})"
	if [ $? -ne 0 ]; then
	    printf "\t... error on file ${f}, will need to reprocess\n"
	    has_error=true
	else
	    printf "Finished with ${binary}\n"
	fi
    done
    cd "${workdir}"/..
}
export -f analyze
