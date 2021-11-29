source scripts/tag.sh

sudo docker run --name superset_disasm_${TAG}_tests  \
     -v /Volumes/corpus/x86-binaries/:/home/opam/workspace/x86-binaries \
     -v /Volumes/corpus/x86_64-binaries/:/home/opam/workspace/x86_64-binaries \
     -v /Volumes/corpus/arm-binaries/:/home/opam/workspace/arm-binaries \
     superset_disasm:${TAG} ./run_tests.sh

sudo docker run --name superset_disasm_flambda_${TAG}_tests \
     -v /Volumes/corpus/x86-binaries/:/home/opam/workspace/x86-binaries \
     -v /Volumes/corpus/x86_64-binaries/:/home/opam/workspace/x86_64-binaries \
     -v /Volumes/corpus/arm-binaries/:/home/opam/workspace/arm-binaries \
    superset_disasm_flambda:${TAG} ./run_tests.sh

