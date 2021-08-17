#!/bin/bash
eval $(opam config env)
source scripts/analyze.sh
#export SUPERSET_FRONTEND=1
analyze ${HOME}/workspace/x86-binaries/elf/binutils/gcc_binutils_32_O3_readelf "TrimLimitedClamped,FixpointFreevarSSA,FixpointGrammar" 

analyze ${HOME}/workspace/x86-binaries/elf/binutils/icc_binutils_32_O2_elfedit "TrimFixpointTails" 

analyze ${HOME}/workspace/x86-binaries/elf/findutils/gcc_findutils_32_O0_bigram "TrimLimitedClamped,FixpointGrammar,FixpointFreevarSSA" 

analyze ${HOME}/workspace/x86-binaries/elf/binutils/icc_binutils_32_O3_cxxfilt "TrimLimitedClamped,FixpointGrammar,FixpointFreevarSSA" 
