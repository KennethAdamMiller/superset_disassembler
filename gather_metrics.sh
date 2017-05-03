rm metrics.txt
find ~/workspace/x86_64-binaries/elf/binutils -type f -exec zsh -c 'echo $(basename {}) ; echo $(basename {}) >> metrics.txt ; ~/workspace/superset_disassembler/superset_disasm.native --ground_truth {} --target "/Users/kennethadammiller/workspace/x86_64-binaries/elf_stripped/binutils/$(basename {})" >> metrics.txt' \;

find ~/workspace/x86_64-binaries/elf/coreutils -type f -exec zsh -c 'echo $(basename {}) ; echo $(basename {}) >> metrics.txt ; ~/workspace/superset_disassembler/superset_disasm.native --ground_truth {} --target "/Users/kennethadammiller/workspace/x86_64-binaries/elf_stripped/coreutils/$(basename {})" >> metrics.txt' \;

find ~/workspace/x86_64-binaries/elf/findutils -type f -exec zsh -c 'echo $(basename {}) ; echo $(basename {}) >> metrics.txt ; ~/workspace/superset_disassembler/superset_disasm.native --ground_truth {} --target "/Users/kennethadammiller/workspace/x86_64-binaries/elf_stripped/findutils/$(basename {})" >> metrics.txt' \;

cat metrics.txt| grep "vertices after trimming" | cut -d " " -f6 > total_removed.txt
cat metrics.txt| grep "Total instructions" | cut -d " " -f4 >  final_total.txt
cat metrics.txt| grep "superset_cfg_of_mem" | grep length | cut -d " " -f3 > mem_size.txt
cat metrics.txt| grep "raw_superset_to_map" | grep length | cut -d " " -f3 > original_superset.txt
cat metrics.txt| grep "Actual function entrances" | cut -d " " -f4 > total_functions.txt
cat metrics.txt| grep "Occlusion" | cut -d " " -f2 > occlusion.txt
cat metrics.txt| grep "False negatives" | cut -d " " -f3 > false_negatives.txt
