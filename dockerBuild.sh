echo "Usage: $0 <BAPVERSION>(${1})"
BAPVERSION=${1}
if [ -z ${1} ]; then BAPVERSION=2.1.0; fi

IMG_TGT=superset_disasm:$(git rev-parse --abbrev-ref HEAD)-$(git rev-parse --short HEAD)_${BAPVERSION}
echo "Building ${IMG_TGT} with BAPVERSION=${BAPVERSION}"
sudo docker build . -f Dockerfile \
     -t  ${IMG_TGT} \
     ${DOCKER_BLD_ARGS} \
     --build-arg BAPVERSION=${BAPVERSION} 
