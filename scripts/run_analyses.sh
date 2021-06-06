#!/bin/bash

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
	count=0
	cat ../${src}.txt | while read f ; do   
            name="./$(basename "${f}").metrics"
	    if [[ (! -f "${name}") || (-z $(cat "${name}" | grep "True positives")) ]]; then
		echo "Processing ${f} for ${1}${src}${2}"
		rm -f "${name}"
		${disasm_dir}/superset_disasm.native --target="${f}" --ground_truth_bin="${unstripped}/$(basename "${f}")" --save_addrs --enable_feature="${1}" --rounds=2 >> "${name}";
		if [ $? -ne 0 ]; then
		    printf "\t... error on file ${f}, will need to reprocess\n"
		    has_error=true
		fi
	    fi
	    count = $(( ${count}+1 ))
	    printf "Finished with ${count} of ${total}\n"
	done
    done
    ${disasm_dir}/scripts/collect_results.sh
    popd
}

#j1= analyze "" "" "0.99"& 
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
#j0= analyze "TrimLimitedClamped,TrimFixpointGrammar" "0.99" &
j1= analyze "TrimLimitedClamped,TrimFixpointSSA,TrimFixpointGrammar" &
#j2= analyze "TrimLimitedClamped,TrimFixpointSSA" "0.990" &
#j4= analyze "TrimLimitedClamped,TrimFixpointGrammar" "0.98" &
#j5= analyze "TrimLimitedClamped,TrimFixpointGrammar" "0.97" &
#j6= analyze "TrimLimitedClamped,TrimFixpointGrammar" "0.90" &
##j5= analyze "Grammar convergent" "" &
#wait ${j1}
#wait ${j2}
#wait ${j99}
#wait ${j4}
#wait ${j5}
#wait ${j6}
#j1= analyze "TrimLimitedClamped,TrimFixpointGrammar" "0.80" &
#j2= analyze "TrimLimitedClamped,TrimFixpointGrammar" "0.70" &
#j3= analyze "TrimLimitedClamped,TrimFixpointGrammar" "0.50" &
#wait ${j1}
#wait ${j2}
#wait ${j3}


#j3= analyze "TrimLimitedClamped,TrimFixpointGrammar" "0.990" &
wait ${j1}
#wait ${j2}
#wait ${j3}
#analyze "All Instruction invariants" "TrimLimitedClamped,FixpointGrammar" "0.99" 
