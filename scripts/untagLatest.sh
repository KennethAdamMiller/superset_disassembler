source scripts/tag.sh
source scripts/feature_suffix.sh
source scripts/project.sh

docker rmi ${PROJ_IMG}:latest-${FSUFFIX}
