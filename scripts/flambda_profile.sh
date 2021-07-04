#!/bin/bash
export TODO=?

eval $(opam config env)
#TODO want to reuse run_analysis script
source analyze.sh
analyze "TrimLimitedClamped,FixpointFreevarSSA,FixpointGrammar" ${HOME}/workspace/x86-binaries/elf/binutils/gcc_binutils_32_O3_readelf

analyze "TrimFixpointTails" ${HOME}/workspace/x86-binaries/elf/binutils/icc_binutils_32_O2_elfedit

analyze "TrimLimitedClamped,FixpointGrammar,FixpointFreevarSSA" ${HOME}/workspace/x86-binaries/elf/findutils/gcc_findutils_32_O0_bigram

analyze "TrimLimitedClamped,FixpointGrammar,FixpointFreevarSSA" ${HOME}/workspace/x86-binaries/elf/binutils/icc_binutils_32_O3_cxxfilt
