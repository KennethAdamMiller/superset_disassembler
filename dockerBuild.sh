echo "Usage: $0 <BAPVERSION>(${1})"
BAPVERSION=${1}
if [ -z ${1} ]; then BAPVERSION=2.3.0; fi

source scripts/tag.sh
source scripts/feature_suffix.sh
IMG_TGT=superset_disasm_${FSUFFIX}:${TAG}
echo "Building ${IMG_TGT} with BAPVERSION=${BAPVERSION}"
sudo docker build . -f Dockerfile \
     -t  ${IMG_TGT} \
     ${DOCKER_BLD_ARGS} \
     --build-arg BAPVERSION=${BAPVERSION} 
