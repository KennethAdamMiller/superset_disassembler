sudo docker build . -f Dockerfile -t superset_disasm:$(git branch --show-current)-$(git rev-parse --short HEAD)
