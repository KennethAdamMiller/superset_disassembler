#!/bin/bash
echo "Usage: $0 <features> <numbinaries>"
export features=${1}
export testsize=${2}
if [ -z ${testsize} ]; then testsize=9999; fi
rm -f metrics.txt
rm -f final_total.txt
rm -f mem_size.txt
rm -f original_superset.txt
rm -f total_functions.txt
rm -f occlusion.txt
rm -f false_negatives.txt
rm -f reduced_occlusion.txt
find results -size 0 -exec rm {} \;

source ./scripts/analyze.sh "" ${features}

bindir=${HOME}/workspace/
#command=' [[ -f results/$(basename {.}).metrics ]] || ~/workspace/superset_disassembler/superset_disasm.native --checkpoint=Export --ground_truth {.} --target {.} --enable_feature="${1}" --rounds=2 --collect_reports >> results/$(basename {.})_metrics.txt '
#TrimLimitedClamped,TrimFixpointSSA,TrimFixpointGrammar
command=' analyze {.} ${features} '
echo "command=${command}"
run() {
    jobs=$1
    subdir=$2
    echo "Running ${subdir} with ${jobs} jobs"
    find ${bindir}/${subdir} -type f -print | grep -v "*.graph" | grep -v "*.map" | head -n ${testsize} | parallel -u -j${jobs} ${command}
}

export jobs=$(( $(nproc --a) + 1 ))
run ${jobs} x86_64-binaries/elf/coreutils

run ${jobs} x86_64-binaries/elf/findutils

run ${jobs} x86-binaries/elf/coreutils

run ${jobs} x86-binaries/elf/findutils

mjobs=$((($(getconf _PHYS_PAGES) * $(getconf PAGE_SIZE) / (1024 * 1024 * 1024)) / 5))
jobs=$(dc -e "[${jobs}]sM ${mjobs}d ${jobs}<Mp")
run ${jobs} x86_64-binaries/elf/binutils

run ${jobs} x86-binaries/elf/binutils

#find ~/workspace/x86_64-binaries/speccpu2000 -name "*.exe" -type f -exec zsh -c 'echo $(basename {}) ; echo $(basename {}) >> metrics.txt ; ~/workspace/superset_disassembler/superset_disasm.native --ground_truth {} --target "/Users/kennethadammiller/workspace/x86-binaries/stripped_speccpu2000/$(basename {})" >> metrics.txt' \;

./scripts/collect_results.sh
