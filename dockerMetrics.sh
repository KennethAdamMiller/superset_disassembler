features=$1
if [ -z ${features} ]; then features=TrimLimitedClamped; fi
testsize=$2
if [ -z ${testsize} ]; then testsize=3; fi
from=$3
if [ -z ${from} ]; then from=analyses; fi
TAG=$(git rev-parse --abbrev-ref HEAD)-$(git rev-parse --short HEAD)
args=$(echo "${features}" | md5sum)
name="superset_disasm_metrics_${TAG}_${args:0:6}"
sudo time docker run --name ${name} \
     -v ${HOME}/workspace/cache:/home/opam/workspace/cache \
     -v ${HOME}/workspace/x86-binaries/:/home/opam/workspace/x86-binaries \
     -v ${HOME}/workspace/x86_64-binaries/:/home/opam/workspace/x86_64-binaries \
     superset_disasm_flambda:${TAG} \
     scripts/run_metrics.sh ${features} ${testsize} ${from}

sudo docker commit ${name} ${name}

#No longer keep the corpora in the image, only the computed metrics
#sudo time docker build . -f Dockerfile.metrics \
#     --build-arg testsize=${testsize} \
#     --build-arg TAG=$(git branch --show-current)-$(git rev-parse --short HEAD) \
#     --build-arg features=${features} \
#     -t superset_disasm_metrics:${TAG}
