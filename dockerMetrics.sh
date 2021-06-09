testsize=$1
if [ -z ${testsize} ]; then testsize=3; fi
sudo time docker build . -f Dockerfile.metrics --build-arg testsize=${testsize} --build-arg TAG=$(git branch --show-current)-$(git rev-parse --short HEAD) -t superset_disasm_metrics:$(git branch --show-current)-$(git rev-parse --short HEAD)-test
