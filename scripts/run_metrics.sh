#!/bin/bash
rm -f metrics.txt
rm -f final_total.txt
rm -f mem_size.txt
rm -f original_superset.txt
rm -f total_functions.txt
rm -f occlusion.txt
rm -f false_negatives.txt
rm -f reduced_occlusion.txt
mkdir -p results
find results -size 0 -exec rm {} \;

analyze() {
    src=binaries
    workdir="${1}${src}${2}_results"
    unstripped=${HOME}/workspace/unstripped/
    disasm_dir=${HOME}/workspace/superset_disasm/
#    rm -rf "${workdir}"
    mkdir "${workdir}"
    pushd "${workdir}"
    has_error=true
    while ${has_error}; do
	has_error=false
	total=$(wc -l ../${src}.txt)
	#count=0
	#cat ../${src}.txt | while read f ; do   
            name="./$(basename "${1}").metrics"
	    if [[ (! -f "${workdir}/${name}") || (-z $(cat "${workdir}/${name}" | grep "True positives")) ]]; then
		echo "Processing ${f} for ${1}${src}${2}"
		rm -f "${name}"
		time ${disasm_dir}/superset_disasm.native --target="${1}" --ground_truth_bin="${unstripped}/$(basename "${1}")" --save_addrs --checkpoint=Export --enable_feature="${2}" --rounds=2 --collect_reports >> "${workdir}/${name}";
		if [ $? -ne 0 ]; then
		    printf "\t... error on file ${f}, will need to reprocess\n"
		    has_error=true
		fi
	    fi
	    #count=$(( ${count}+1 ))
	    printf "Finished with of ${name}\n"
	#done
    done
    ${disasm_dir}/scripts/collect_results.sh
    popd
}


bindir=${HOME}/workspace/
jobs=6
#command=' [[ -f results/$(basename {.}).metrics ]] || ~/workspace/superset_disassembler/superset_disasm.native --checkpoint=Export --ground_truth {.} --target {.} --enable_feature="${1}" --rounds=2 --collect_reports >> results/$(basename {.})_metrics.txt '
command=' analyze {.} "TrimLimitedClamped,TrimFixpointSSA,TrimFixpointGrammar" '
run() {
    jobs=$1
    subdir=$2
    find ${bindir}/${subdir} -type f -print | grep -v "*.graph" | grep -v "*.map" | parallel -j${jobs} ${command}
}

run ${jobs} x86_64-binaries/elf/coreutils

run ${jobs} x86_64-binaries/elf/findutils

run ${jobs} x86-binaries/elf/coreutils

run ${jobs} x86-binaries/elf/findutils

jobs=3
run ${jobs} x86_64-binaries/elf/binutils

run ${jobs} x86-binaries/elf/binutils

#find ~/workspace/x86_64-binaries/speccpu2000 -name "*.exe" -type f -exec zsh -c 'echo $(basename {}) ; echo $(basename {}) >> metrics.txt ; ~/workspace/superset_disassembler/superset_disasm.native --ground_truth {} --target "/Users/kennethadammiller/workspace/x86-binaries/stripped_speccpu2000/$(basename {})" >> metrics.txt' \;

./collect_results.sh
