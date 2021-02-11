#!/bin/bash

analyze() {
    src=binaries
    workdir="${2}${src}${3}_results"
    unstripped=${HOME}/workspace/unstripped/
    disasm_dir=${HOME}/workspace/superset_disasm/
#    rm -rf "${workdir}"
    mkdir "${workdir}"
    pushd "${workdir}"
    has_error=true
    while ${has_error}; do
	has_error=false
	cat ../${src}.txt | while read f ; do   
            name="./$(basename "${f}").metrics"
	    if [[ ! -f "${name}" ]]; then
		    echo "no file!"
	    fi	
	    if [[ (-z $(cat "${name}" | grep "True positives")) ]]; then
		    echo "missing true positives"
	    fi	
	    if [[ (! -f "${name}") || (-z $(cat "${name}" | grep "True positives")) ]]; then
		echo "Processing: ${f}"
		${disasm_dir}/superset_disasm.native --phases="${1}" --target="${f}" --ground_truth_bin="${unstripped}/$(basename "${f}")" --save_addrs --enable_feature="${2}" --rounds=2 --tp_threshold="${3}" >> "${name}";
		if [ $? -ne 0 ]; then
		    printf "\t... error on file ${f}, will need to reprocess\n"
		    has_error=true
		fi
	    fi
	done
    done
    ${disasm_dir}/scripts/collect_results.sh
    popd
}

#j1= analyze "All Instruction invariants" "" "0.99"& 
#j2= analyze "Target_out_of_bounds" "" "0.99"& 
#j3= analyze "Target_is_bad" "" "0.99"& 
#j4= analyze "Invalid memory accesses" "" "0.99"& 
#j5= analyze "Target_within_body" "" "0.99"&
#j6= analyze "Non instruction opcode" "" "0.99"& 
#wait ${j1}
#wait ${j2}
#wait ${j3}
#wait ${j4}
#wait ${j5}
#wait ${j6}
#
#j1= analyze "Strongly Connected Component Data" "" "0.99"& 
#j2= analyze "Cross Layer Invalidation" "" "0.99"& 
j3= analyze "All Instruction invariants" "TrimLimitedClamped,TrimFixpointGrammar" "0.99" &
j4= analyze "All Instruction invariants" "TrimLimitedClamped,TrimFixpointGrammar" "0.98" &
j5= analyze "All Instruction invariants" "TrimLimitedClamped,TrimFixpointGrammar" "0.97" &
j6= analyze "All Instruction invariants" "TrimLimitedClamped,TrimFixpointGrammar" "0.90" &
##j5= analyze "Grammar convergent" "" &
#wait ${j1}
#wait ${j2}
wait ${j3}
wait ${j4}
wait ${j5}
wait ${j6}
j1= analyze "All Instruction invariants" "TrimLimitedClamped,TrimFixpointGrammar" "0.80" &
j2= analyze "All Instruction invariants" "TrimLimitedClamped,TrimFixpointGrammar" "0.70" &
j3= analyze "All Instruction invariants" "TrimLimitedClamped,TrimFixpointGrammar" "0.50" &
wait ${j1}
wait ${j2}
wait ${j3}

j1= analyze "All Instruction invariants" "TrimLimitedClamped,TrimFixpointSSA,TrimFixpointGrammar" "0.990" &
j2= analyze "All Instruction invariants" "TrimLimitedClamped,TrimFixpointSSA" "0.990" &
j3= analyze "All Instruction invariants" "TrimLimitedClamped,TrimFixpointGrammar" "0.990" &
wait ${j1}
wait ${j2}
wait ${j3}
#analyze "All Instruction invariants" "TrimLimitedClamped,FixpointGrammar" "0.99" 
