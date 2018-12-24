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

bindir=${HOME}/workspace/
jobs=6
command=' [[ -f results/$(basename {.})_metrics.txt ]] || ~/workspace/superset_disassembler/superset_disasm.native --checkpoint=Export --ground_truth {.} --target {.} >> results/$(basename {.})_metrics.txt '
analyze() {
  subdir=$1
  find ${bindir}/${subdir} -type f -print | grep -v "*.graph" | grep -v "*.map" | parallel -j${jobs} ${command}
}

analyze x86_64-binaries/elf/coreutils

analyze x86_64-binaries/elf/findutils

analyze x86-binaries/elf/coreutils

analyze x86-binaries/elf/findutils

jobs=3
analyze x86_64-binaries/elf/binutils

analyze x86-binaries/elf/binutils

#find ~/workspace/x86_64-binaries/speccpu2000 -name "*.exe" -type f -exec zsh -c 'echo $(basename {}) ; echo $(basename {}) >> metrics.txt ; ~/workspace/superset_disassembler/superset_disasm.native --ground_truth {} --target "/Users/kennethadammiller/workspace/x86-binaries/stripped_speccpu2000/$(basename {})" >> metrics.txt' \;

./collect_results.sh
