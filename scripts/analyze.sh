#!/bin/bash

compute_disasm() {
    import_name=$1
    export_name=$2
    gt_bin=$3
    invariants=$4
    analyses=$5
    features=$6
    rounds=$7
    args=""
    if [[ $SUPERSET_FRONTEND ]]; then
	args=" --target=./$(basename ${gt_bin}) --ground_truth_bin=${gt_bin} --save_gt --rounds=${rounds}"
	if [[ ! (-z $import_name) ]]; then
	    args+=" --import=${import_name}"
	fi
	if [[ ! (-z $export_name) ]]; then
	    args+=" --export=${export_name}"
	fi
	if [[ ! (-z ${invariants}) ]]; then
	    args+=" --invariants=${invariants}"
	fi
	if [[ ! (-z ${analyses}) ]]; then
	    args+=" --analyses=${analyses}"
	fi
	if [[ ! (-z ${features}) ]]; then
	    args+=" --enable_feature=${features}"
	fi
	echo "time superset_disasm ${args}"
	time superset_disasm ${args}
    else
	args="./$(basename ${gt_bin}) --ground_truth_bin=${gt_bin} --rounds=${rounds} "
	if [[ ! (-z ${invariants}) ]]; then
	    args+=" --invariants=${invariants}"
	fi
	if [[ ! (-z ${analyses}) ]]; then
	    args+=" --analyses=${analyses}"
	fi
	if [[ ! (-z ${features}) ]]; then
	    args+=" --features=${features}"
	fi
	echo "time bap superset_disasm ${args}"
	time bap superset_disasm ${args}
    fi
}
export -f compute_disasm

analyze() {
    export binary=${1}
    export features=${2}
    export from=${3}
    echo "Usage: $0 <bin>=${binary} <features>=${features}"
    cachedir=${HOME}/workspace/cache
    mkdir -p ${cachedir}
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
        name="${workdir}/$(basename ${1})_${features}.metrics"
	if [[ (! -f "${workdir}/${name}") || (-z $(cat "${workdir}/${name}" | grep "True positives")) ]]; then
	    echo "name=${name}"
	    echo "Processing ${workdir}/$(basename ${1}) for ${features}"
	    rm -f "${name}"
	    cp "${1}" ./
	    strip "./$(basename ${1})"
	    if [[ (! -f "${cachedir}/$(basename ${1})_gt.txt") ]]; then
		if [[ $SUPERSET_FRONTEND ]]; then
		    echo "Computing ground truth"
		    compute_disasm "" "" "${1}" "" "" "" 1
		fi
	    fi
	    if [[ (! -f "${cachedir}/$(basename ${1})_superset.graph.gz") ]]; then
		if [[ $SUPERSET_FRONTEND ]]; then
		    echo "Computing raw superset"
		    compute_disasm "" "superset" "${1}" "" "" "" 1
		fi
	    fi
	    if [[ (! -f "${cachedir}/$(basename ${1})_invariants.graph.gz") ]]; then
		if [[ $SUPERSET_FRONTEND ]]; then
		    echo "Computing superset - invariants"
		    compute_disasm "superset" "invariants" "${1}" 1 "" "" 1
		fi
	    fi
	    if [[ (! -f "${cachedir}/$(basename ${1})_analyses.graph.gz") ]]; then
		if [[ $SUPERSET_FRONTEND ]]; then
		    echo "Computing superset - invariants - analyses"
		    compute_disasm "invariants" "analyses" "${1}" "" 1 "" 1
		fi
	    fi
	    if [[ (! -f "./$(basename ${1})_${from}.graph") && (-f "./$(basename ${1})_${from}.graph.gz") ]]; then
	       gzip -d "./$(basename ${1})_${from}.graph.gz"
	    fi
	    echo "Computing convergence"
	    compute_disasm "${from}" "${features}" "${1}" "" "" "${features}" 6
	    rm -f "./$(basename ${1})"
	    if [ $? -ne 0 ]; then
		printf "\t... error on file ${f}, will need to reprocess\n"
		has_error=true
	    fi
	    if [[ -f "./$(basename ${1})_superset.graph" ]]; then
		gzip  "./$(basename ${1})_superset.graph"
	    fi
	    if [[ -f "./$(basename ${1})_invariants.graph" ]]; then
		gzip "./$(basename ${1})_invariants.graph"
	    fi
	    if [[ -f "./$(basename ${1})_${from}.graph" ]]; then
		gzip "./$(basename ${1})_${from}.graph"
	    fi
	    if [[ -f "./$(basename ${1})_${features}.graph" ]]; then
		gzip "./$(basename ${1})_${features}.graph"
	    fi
	fi
	printf "Finished with ${name}\n"
    done
    cd "${workdir}"/..
}
export -f analyze
