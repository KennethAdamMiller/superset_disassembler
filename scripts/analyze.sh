#!/bin/bash

export binary=${1}
export features=${2}
echo "Usage: $0 <features>=${features}"
cachedir=${HOME}/workspace/cache
mkdir -p ${cachedir}

analyze() {
    src=$(git rev-parse --abbrev-ref HEAD)-$(git rev-parse --short HEAD)
    disasm_dir=${HOME}/workspace/superset_disasm/
    workdir="${disasm_dir}/${src}_results"
#    rm -rf "${workdir}"
    mkdir -p "${workdir}"
    echo "${features}" >> features.txt
    cd "${cachedir}"
    pwd
    has_error=true
    while ${has_error}; do
	has_error=false
        name="${workdir}/$(basename ${1}).metrics"
	if [[ (! -f "${workdir}/${name}") || (-z $(cat "${workdir}/${name}" | grep "True positives")) ]]; then
	    echo "Processing ${1} for ${2}"
	    echo "name=${name}"
	    rm -f "${name}"
	    cp "${1}" ./
	    strip "./$(basename ${1})"
	    if [[ (! -f "${cachedir}/$(basename ${1})_gt.txt") ]]; then
		echo "Computing ground truth"
		time ${disasm_dir}/superset_disasm.native --target="./$(basename ${1})" --ground_truth_bin="${1}" --save_gt --phases="" --analyses="" --enable_feature="" --rounds=1 > /dev/null;
	    fi
	    if [[ (! -f "${cachedir}/$(basename ${1})_superset.graph.gz") ]]; then
		echo "Computing raw superset"
		time ${disasm_dir}/superset_disasm.native --target="./$(basename ${1})" --export=superset --phases="" --analyses="" --enable_feature="" --rounds=1 > /dev/null;
	    fi
	    if [[ (! -f "${cachedir}/$(basename ${1})_invariants.graph.gz") ]]; then
		echo "Computing superset - invariants"
		time ${disasm_dir}/superset_disasm.native --target="./$(basename ${1})" --import=superset --export=invariants --analyses="" --enable_feature="" --rounds=1 > /dev/null;
	    fi
	    if [[ (! -f "${cachedir}/$(basename ${1})_analyses.graph.gz") ]]; then
		echo "Computing superset - invariants - analyses"
		time ${disasm_dir}/superset_disasm.native --target="./$(basename ${1})" --import=invariants --export=analyses --phases="" --enable_feature="" --rounds=1 > /dev/null;
	    fi
	    echo "Computing convergence"
	    time ${disasm_dir}/superset_disasm.native --target="./$(basename ${1})" --import=analyses --export=features --ground_truth_bin="${1}" --enable_feature="${features}" --rounds=6 --collect_reports >> "${name}";
	    rm -f "./$(basename ${1})"
	    if [ $? -ne 0 ]; then
		printf "\t... error on file ${f}, will need to reprocess\n"
		has_error=true
	    fi
	    gzip "./$(basename ${1})_superset.graph"
	    gzip "./$(basename ${1})_invariants.graph"
	    gzip "./$(basename ${1})_analyses.graph"
	    gzip "./$(basename ${1})_features.graph"
	fi
	printf "Finished with ${name}\n"
    done
    cd "${workdir}"
    ${disasm_dir}/scripts/collect_results.sh
    cd ..
}
export -f analyze
