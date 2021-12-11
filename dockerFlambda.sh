echo "Usage: $0"
TAG=$(git rev-parse --abbrev-ref HEAD)-$(git rev-parse --short HEAD)
echo "TAG=${TAG}"
BAPVERSION=${1}
if [ -z ${1} ]; then BAPVERSION=2.3.0; fi

sudo docker build . -f Dockerfile.flambda \
     --build-arg TAG=${TAG} \
     --build-arg BAPVERSION=${BAPVERSION} \
     -t superset_disasm_flambda:${TAG}

docker tag superset_disasm_flambda:${TAG} superset_disasm_flambda:latest
