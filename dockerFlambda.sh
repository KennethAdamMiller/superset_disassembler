echo "Usage: $0"
TAG=$(git rev-parse --abbrev-ref HEAD)-$(git rev-parse --short HEAD)
echo "TAG=${TAG}"
BAPVERSION=${1}
if [ -z ${1} ]; then BAPVERSION=2.3.0; fi

sudo docker build . -f Dockerfile.flambda \
     --build-arg TAG=${TAG} \
     --build-arg BAPVERSION=${BAPVERSION} \
     -t superset_disasm_flambda:${TAG}

sudo docker run \
     -v ${HOME}/workspace/cache:/home/opam/workspace/cache \
     -v ${HOME}/workspace/x86-binaries/:/home/opam/workspace/x86-binaries \
     -v ${HOME}/workspace/x86_64-binaries/:/home/opam/workspace/x86_64-binaries \
     superset_disasm_flambda:${TAG} \
     ./scripts/flambda_profile.sh
