source scripts/tag.sh
source scripts/feature_suffix.sh

sudo docker run --name superset_disasm_${TAG}_tests  \
     -v /Volumes/corpus/x86-binaries/:/Volumes/corpus/workspace/x86-binaries \
     -v /Volumes/corpus/x86_64-binaries/:/Volumes/corpus/workspace/x86_64-binaries \
     -v /Volumes/corpus/arm-binaries/:/Volumes/corpus/workspace/arm-binaries \
     superset_disasm:${TAG}-${FSUFFIX} ./run_tests.sh


