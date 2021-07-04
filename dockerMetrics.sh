features=$1
if [ -z ${features} ]; then features=TrimLimitedClamped; fi
testsize=$2
if [ -z ${testsize} ]; then testsize=3; fi
TAG=$(git rev-parse --abbrev-ref HEAD)-$(git rev-parse --short HEAD)
sudo time docker run --name superset_disasm_metrics_${TAG} \
     -v ${HOME}/workspace/cache:/home/opam/workspace/cache \
     -v ${HOME}/workspace/x86-binaries/:/home/opam/workspace/x86-binaries \
     -v ${HOME}/workspace/x86_64-binaries/:/home/opam/workspace/x86_64-binaries \
     superset_disasm_flambda:${TAG} \
     scripts/run_metrics.sh ${features} ${testsize}

#No longer keep the corpora in the image, only the computed metrics
#sudo time docker build . -f Dockerfile.metrics \
#     --build-arg testsize=${testsize} \
#     --build-arg TAG=$(git branch --show-current)-$(git rev-parse --short HEAD) \
#     --build-arg features=${features} \
#     -t superset_disasm_metrics:${TAG}
