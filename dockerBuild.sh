sudo docker build . -f Dockerfile.build -t superset_disasm:$(git rev-parse --abbrev-ref HEAD)-$(git rev-parse --short HEAD)
