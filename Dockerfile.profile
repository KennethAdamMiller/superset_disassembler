ARG TAG
FROM superset_disasm:${TAG}
ARG features

WORKDIR $HOME/workspace/superset_disasm
RUN ./scripts/profile.sh