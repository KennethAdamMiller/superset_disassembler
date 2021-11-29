export OCAML_SPACETIME_INTERVAL=1000


make clean && make
time ./superset_disasm.native --target /Volumes/corpus/x86-binaries/elf/binutils/gcc_binutils_32_O3_readelf --rounds=3  --heuristics="TrimLimitedClamped,FixpointFreevarSSA,FixpointGrammar"  --ground_truth_bin=/Volumes/corpus/x86-binaries/elf/binutils/gcc_binutils_32_O3_readelf --tp_threshold=0.98 2>&1 gcc_binutils_32_O3_readelf.spacetime

time ./superset_disasm.native --target /Volumes/corpus/x86-binaries/elf/binutils/icc_binutils_32_O2_elfedit --rounds=3  --heuristics="TrimFixpointTails"  --ground_truth_bin=/Volumes/corpus/x86-binaries/elf/binutils/icc_binutils_32_O2_elfedit --tp_threshold=0.96 2>&1 icc_binutils_32_O2_elfedit.spacetime

time ./superset_disasm.native --target /Volumes/corpus/x86-binaries/elf/findutils/gcc_findutils_32_O0_bigram --rounds=3  --heuristics="TrimLimitedClamped,TrimFixpointGrammar,TrimFixpointSSA" --analyses="Strongly Connected Component Data"  --ground_truth_bin=/Volumes/corpus/x86-binaries/elf/findutils/gcc_findutils_32_O0_bigram 2>&1 gcc_findutils_32_O0_bigram.spacetime
