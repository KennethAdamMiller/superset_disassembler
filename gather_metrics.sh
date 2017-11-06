rm metrics.txt
rm final_total.txt
rm mem_size.txt
rm original_superset.txt
rm total_functions.txt
rm occlusion.txt
rm false_negatives.txt
rm reduced_occlusion.txt
mkdir -p results


bindir=/Volumes/FAGFD/
jobs=3
command='~/workspace/superset_disassembler/superset_disasm.native --checkpoint=Export --ground_truth {.} --target {.} >> results/$(basename {.})_metrics.txt'
analyze() {
  subdir=$1
  find ${bindir}/${subdir} -type f -print | grep -v "*.graph" | grep -v "*.map" | parallel -j${jobs} ${command}
}

analyze x86_64-binaries/elf/binutils

analyze x86_64-binaries/elf/coreutils

analyze x86_64-binaries/elf/findutils

analyze x86-binaries/elf/binutils

analyze x86-binaries/elf/coreutils

analyze x86-binaries/elf/findutils

#find ~/workspace/x86_64-binaries/speccpu2000 -name "*.exe" -type f -exec zsh -c 'echo $(basename {}) ; echo $(basename {}) >> metrics.txt ; ~/workspace/superset_disassembler/superset_disasm.native --ground_truth {} --target "/Users/kennethadammiller/workspace/x86-binaries/stripped_speccpu2000/$(basename {})" >> metrics.txt' \;

./collect_results.sh
