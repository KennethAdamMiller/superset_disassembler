FROM binaryanalysisplatform/bap

USER root
RUN mkdir -p /home/opam/workspace/superset_disasm
COPY ./ /home/opam/workspace/superset_disasm/
RUN chown -R opam:opam /home/opam/
USER opam

WORKDIR /home/opam/workspace/superset_disasm
RUN rm setup.data ; eval `opam config env` ; opam install ounit fmt logs ; make clean ; make ; opam pin add superset_disasm ./ -y --use-internal-solver
RUN sudo apt-get update
RUN opam depext --install bap-byteweight-frontend
