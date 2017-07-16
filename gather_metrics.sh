rm metrics.txt
rm final_total.txt
rm mem_size.txt
rm original_superset.txt
rm total_functions.txt
rm occlusion.txt
rm false_negatives.txt
rm reduced_occlusion.txt

find ~/workspace/x86_64-binaries/elf/binutils -type f -exec zsh -c 'echo $(basename {}) ; echo $(basename {}) >> metrics.txt ; ~/workspace/superset_disassembler/superset_disasm.native --ground_truth {} --target "/Users/kennethadammiller/workspace/x86_64-binaries/elf_stripped/binutils/$(basename {})" >> metrics.txt' \;

find ~/workspace/x86_64-binaries/elf/coreutils -type f -exec zsh -c 'echo $(basename {}) ; echo $(basename {}) >> metrics.txt ; ~/workspace/superset_disassembler/superset_disasm.native --ground_truth {} --target "/Users/kennethadammiller/workspace/x86_64-binaries/elf_stripped/coreutils/$(basename {})" >> metrics.txt' \;

find ~/workspace/x86_64-binaries/elf/findutils -type f -exec zsh -c 'echo $(basename {}) ; echo $(basename {}) >> metrics.txt ; ~/workspace/superset_disassembler/superset_disasm.native --ground_truth {} --target "/Users/kennethadammiller/workspace/x86_64-binaries/elf_stripped/findutils/$(basename {})" >> metrics.txt' \;

find ~/workspace/x86-binaries/elf/binutils -type f -exec zsh -c 'echo $(basename {}) ; echo $(basename {}) >> metrics.txt ; ~/workspace/superset_disassembler/superset_disasm.native --ground_truth {} --target "/Users/kennethadammiller/workspace/x86-binaries/elf_stripped/binutils/$(basename {})" >> metrics.txt' \;

find ~/workspace/x86-binaries/elf/coreutils -type f -exec zsh -c 'echo $(basename {}) ; echo $(basename {}) >> metrics.txt ; ~/workspace/superset_disassembler/superset_disasm.native --ground_truth {} --target "/Users/kennethadammiller/workspace/x86-binaries/elf_stripped/coreutils/$(basename {})" >> metrics.txt' \;

find ~/workspace/x86-binaries/elf/findutils -type f -exec zsh -c 'echo $(basename {}) ; echo $(basename {}) >> metrics.txt ; ~/workspace/superset_disassembler/superset_disasm.native --ground_truth {} --target "/Users/kennethadammiller/workspace/x86-binaries/elf_stripped/findutils/$(basename {})" >> metrics.txt' \;



cat metrics.txt| grep "vertices after trimming" | cut -d " " -f6 > total_removed.txt
cat metrics.txt| grep "Total instructions" | cut -d " " -f4 >  final_total.txt
cat metrics.txt| grep "superset_cfg_of_mem" | grep length | cut -d " " -f3 > mem_size.txt
cat metrics.txt| grep "superset_map" | grep length | cut -d " " -f3 > original_superset.txt
cat metrics.txt| grep "Actual function entrances" | cut -d " " -f4 > total_functions.txt
cat metrics.txt| grep "Occlusion" | cut -d " " -f2 > occlusion.txt
cat metrics.txt| grep "Reduced occlusion" | cut -d " " -f3 > reduced_occlusion.txt
cat metrics.txt| grep "False negatives" | cut -d " " -f3 > false_negatives.txt
cat metrics.txt| grep "True positives" | cut -d " " -f3 > true_positives.txt
cat metrics.txt| grep "Number of possible reduced" | cut -d " " -f7 > tp_byte_space.txt

rm binaries.txt
find ~/workspace/x86_64-binaries/elf/binutils -type f -print >> binaries.txt
find ~/workspace/x86_64-binaries/elf/coreutils -type f -print >> binaries.txt
find ~/workspace/x86_64-binaries/elf/findutils -type f -print >> binaries.txt
find ~/workspace/x86-binaries/elf/binutils -type f -print >> binaries.txt
find ~/workspace/x86-binaries/elf/coreutils -type f -print >> binaries.txt
find ~/workspace/x86-binaries/elf/findutils -type f -print >> binaries.txt
cat -n binaries.txt > numbered_binaries.txt
mv numbered_binaries.txt binaries.txt
cat binaries.txt | grep x86 | grep -v _64 | sed '/^$/d' | cut -d " " -f3 | cut -f1 > x86_binaries.txt
cat binaries.txt | grep x86_64 | sed '/^$/d' | cut -f1 > x86_64binaries.txt
cat binaries.txt | grep gcc | sed '/^$/d' | cut -f1 > gcc_binaries.txt
cat binaries.txt | grep icc | sed '/^$/d' | cut -f1 > icc_binaries.txt
