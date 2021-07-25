echo "Usage: $0 <features>"
features=$1
if [ -z ${features} ]; then features=TrimLimitedClamped; fi
TAG=$(git branch --show-current)-$(git rev-parse --short HEAD)
echo "TAG=${TAG}, features=${features}"
sudo docker build . -f Dockerfile.landmarks \
     --build-arg features=${features} \
     --build-arg TAG=${TAG} \
     -v ${HOME}/workspace/cache:/home/opam/workspace/cache \
     -v ${HOME}/workspace/x86-binaries/:/home/opam/workspace/x86-binaries \
     -v ${HOME}/workspace/x86_64-binaries/:/home/opam/workspace/x86_64-binaries \
     -t superset_disasm_landmarks:${TAG}

sudo docker build . -f Dockerfile.spacetime \
     --build-arg features=${features} \
     --build-arg TAG=${TAG} \
     -v ${HOME}/workspace/cache:/home/opam/workspace/cache
     -v ${HOME}/workspace/x86-binaries/:/home/opam/workspace/x86-binaries \
     -v ${HOME}/workspace/x86_64-binaries/:/home/opam/workspace/x86_64-binaries \
     -t superset_disasm_spacetime:${TAG}