source scripts/tag.sh
source scripts/feature_suffix.sh
./dockerBuild.sh
docker tag superset_disasm:${TAG}-${FSUFFIX} ${registry}superset_disasm:${TAG}-${FSUFFIX}
docker push ${registry}superset_disasm:${TAG}-${FSUFFIX}
