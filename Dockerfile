FROM ocaml/opam2:ubuntu-20.04
ARG BAPVERSION=2.3.0
ARG OPAMSWITCH=4.10.0+flambda

RUN sudo apt-get update  \
 && opam update \
 && opam switch create ${OPAMSWITCH} \
 && eval "$(opam env)" \
 && opam remote set-url default https://opam.ocaml.org \
# && opam repo add bap git://github.com/BinaryAnalysisPlatform/opam-repository --all \
 && opam update \
 && opam depext --install bap.${BAPVERSION} --yes -j 1 \
 && opam clean -acrs \
 && rm -rf /home/opam/.opam/4.0[2-8,10] \
 && rm -rf /home/opam/.opam/4.09/.opam-switch/sources/* \
 && rm -rf /home/opam/opam-repository \
 && mkdir -p $HOME/workspace/superset_disasm

USER root
WORKDIR $HOME/workspace/superset_disasm

USER opam
RUN sudo apt-get update && echo 'debconf debconf/frontend select Noninteractive' | sudo debconf-set-selections && sudo apt-get install dc time parallel -y && sudo apt-get install python3-pip -y && pip3 install matplotlib pyzmq
RUN DEBIAN_FRONTEND=noninteractive opam depext --install bap-byteweight-frontend.${BAPVERSION} zmq zmq-async landmarks gnuplot && eval `opam config env` && bap-byteweight update

USER root
COPY ./ /home/opam/workspace/superset_disasm/
RUN chown -R opam:opam /home/opam/workspace/superset_disasm/ && chown opam:opam /home/opam/workspace
RUN rm setup.data ; eval `opam config env` ; opam pin add superset_disasm ./ -y --use-internal-solver ;
USER opam
