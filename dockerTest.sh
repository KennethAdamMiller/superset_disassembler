TAG=$(git rev-parse --abbrev-ref HEAD)-$(git rev-parse --short HEAD)
#sudo docker run \
#     --name superset_disasm_flambda_${TAG}_cache_test \
#     -v ${HOME}/workspace/cache:/home/opam/workspace/cache \
#     -v ${HOME}/workspace/x86-binaries/:/home/opam/workspace/x86-binaries \
#     -v ${HOME}/workspace/x86_64-binaries/:/home/opam/workspace/x86_64-binaries \
#     superset_disasm_flambda:${TAG} \
#     ./functional_tests/test_cache.sh

sudo docker run --name superset_disasm_${TAG}_tests  \
     -v ${HOME}/workspace/cache:/home/opam/workspace/cache \
     -v ${HOME}/workspace/x86-binaries/:/home/opam/workspace/x86-binaries \
     -v ${HOME}/workspace/x86_64-binaries/:/home/opam/workspace/x86_64-binaries \
     superset_disasm:${TAG} ./run_tests.sh

sudo docker run --name superset_disasm_flambda_${TAG}_tests \
     -v ${HOME}/workspace/cache:/home/opam/workspace/cache \
     -v ${HOME}/workspace/x86-binaries/:/home/opam/workspace/x86-binaries \
     -v ${HOME}/workspace/x86_64-binaries/:/home/opam/workspace/x86_64-binaries \
     superset_disasm_flambda:${TAG} ./run_tests.sh

