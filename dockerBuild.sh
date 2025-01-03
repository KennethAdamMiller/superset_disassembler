echo "Usage: $0 <BAPVERSION>(${1})"
BAPVERSION=${1}
if [ -z ${1} ]; then BAPVERSION=2.3.0; fi

source scripts/tag.sh
source scripts/feature_suffix.sh
source vars.sh
IMG_TGT=superset_disasm:${TAG}-${FSUFFIX}

if [[ "$(docker images -q superset_disasm:${TAG}-${FSUFFIX} 2> /dev/null)" == "" ]]; then
	echo "Building ${IMG_TGT} with BAPVERSION=${BAPVERSION}"
	sudo docker build . -f Dockerfile \
	     -t  ${IMG_TGT} \
	     ${DOCKER_BLD_ARGS} \
	     --build-arg SWITCH=${OPAMSWITCH} \
	     --build-arg BAPVERSION=${BAPVERSION} 
fi
docker tag superset_disasm:${TAG}-${FSUFFIX} superset_disasm:latest-${FSUFFIX}
