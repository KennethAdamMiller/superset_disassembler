testsize=$1
if [ -z ${testsize} ]; then testsize=3; fi
features=$2
if [ -z ${features} ]; then features=TrimLimitedClamped; fi
TAG=$(git rev-parse --abbrev-ref HEAD)-$(git rev-parse --short HEAD)
sudo time docker build . -f Dockerfile.metrics \
     --build-arg testsize=${testsize} \
     --build-arg TAG=$(git branch --show-current)-$(git rev-parse --short HEAD) \
     --build-arg features=${features} \
     -v ${HOME}/workspace/cache:/home/opam/workspace/cache \
     -v ${HOME}/workspace/x86-binaries/:/home/opam/workspace/x86-binaries \
     -v ${HOME}/workspace/x86_64-binaries/:/home/opam/workspace/x86_64-binaries \
     -t superset_disasm_metrics:${TAG}
