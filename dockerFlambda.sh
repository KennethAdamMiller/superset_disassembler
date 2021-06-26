echo "Usage: $0 <features>"
TAG=$(git rev-parse --abbrev-ref HEAD)-$(git rev-parse --short HEAD)
echo "TAG=${TAG}"

sudo docker build . -f Dockerfile.flambda \
     --build-arg TAG=${TAG} \
     -t superset_disasm_flambda:${TAG}

sudo docker run \
     -v ${HOME}/workspace/cache:/home/opam/workspace/cache \
     -v ${HOME}/workspace/x86-binaries/:/home/opam/workspace/x86-binaries \
     -v ${HOME}/workspace/x86_64-binaries/:/home/opam/workspace/x86_64-binaries \
     superset_disasm_flambda:${TAG} \
     ./scripts/flambda_profile.sh
