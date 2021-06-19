export TODO=?

eval $(opam config env)
make clean && make
time ./superset_disasm.native --target ${HOME}/workspace/x86-binaries/elf/binutils/gcc_binutils_32_O3_readelf --rounds=3  --enable_feature="TrimLimitedClamped,FixpointFreevarSSA,FixpointGrammar"  --ground_truth_bin=${HOME}/workspace/x86-binaries/elf/binutils/gcc_binutils_32_O3_readelf > gcc_binutils_32_O3_readelf.flambda

time ./superset_disasm.native --target ${HOME}/workspace/x86-binaries/elf/binutils/icc_binutils_32_O2_elfedit --rounds=3  --enable_feature="TrimFixpointTails"  --ground_truth_bin=${HOME}/workspace/x86-binaries/elf/binutils/icc_binutils_32_O2_elfedit > icc_binutils_32_O2_elfedit.flambda

time ./superset_disasm.native --target ${HOME}/workspace/x86-binaries/elf/findutils/gcc_findutils_32_O0_bigram --rounds=3  --enable_feature="TrimLimitedClamped,TrimFixpointGrammar,TrimFixpointSSA" --analyses="Strongly Connected Component Data"  --ground_truth_bin=${HOME}/workspace/x86-binaries/elf/findutils/gcc_findutils_32_O0_bigram > gcc_findutils_32_O0_bigram.flambda
