FROM binaryanalysisplatform/bap

RUN mkdir -p /home/bap/workspace/superset_disasm
COPY ./ /home/bap/workspace/superset_disasm/
USER root
RUN chown -R bap:bap /home/bap/workspace/superset_disasm/
USER bap

WORKDIR /home/bap/workspace/superset_disasm
RUN rm setup.data ; eval `opam config env` ; opam install ounit fmt logs ; make clean ; make ; opam pin add superset_disasm ./ -y --use-internal-solver
RUN sudo apt-get update
RUN opam depext --install bap-byteweight-frontend
