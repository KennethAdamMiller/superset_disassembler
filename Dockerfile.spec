FROM ubuntu:18.04

WORKDIR /home/opam/workspace

##### SPEC CPU 2006
RUN pip install gdown ; sudo apt-get update && sudo apt-get install gcc
USER root
RUN /home/opam/.local/bin/gdown  https://drive.google.com/uc?id=1Pcqp7OfkTvlXn-rtIMDUPh4IjdBrixEs
RUN unzip -q cpu2006-103.zip && rm cpu2006-103.zip && rm -rf __MAXOSX result/* && cd cpu2006-103 && $(printf "yes\n") | ./install.sh
COPY Example-linux64-amd64-gcc41.cfg /home/opam/workspace/cpu2006-103/config/
RUN chown -R opam:opam /home/opam/.local/ && chown -R opam:opam $HOME/workspace
#WORKDIR /home/opam/workspace/cpu2006-103

##### SPEC CPU 2006 performance
WORKDIR $HOME/workspace/cpu2006-103
#RUN unzip -q cpu2006-103.zip && cd cpu2006-103/ && . ./shrc && $(printf "yes\n") | ./install.sh
#COPY Example-linux64-amd64-gcc41.cfg ./cpu2006-103/config/
RUN . ./shrc && ./bin/runspec --noreport --config=Example-linux64-amd64-gcc41.cfg --action=build gcc perlbench bzip2 gcc mcf gobmk hmmer sjeng libquantum h264ref omnetpp astar xalancbmk


