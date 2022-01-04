source scripts/tag.sh
source scripts/feature_suffix.sh

docker run --rm \
     -v /Volumes/corpus/x86-binaries/:/Volumes/corpus/x86-binaries \
     -v /Volumes/corpus/x86_64-binaries/:/Volumes/corpus/x86_64-binaries \
     -v /Volumes/corpus/arm-binaries/:/Volumes/corpus/arm-binaries \
     -v /Volumes/caches/${TAG}:/home/opam/.cache/ \
     superset_disasm:${TAG}-${FSUFFIX} ./scripts/verify_cache.sh #bap superset_cache --is_present $1


