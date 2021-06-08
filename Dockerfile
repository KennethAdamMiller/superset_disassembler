FROM binaryanalysisplatform/bap:2.1.0

WORKDIR /home/opam/workspace
ENV PATH="/home/opam/.opam/4.09/bin:"$PATH

USER root
RUN git clone https://github.com/BinaryAnalysisPlatform/x86-binaries.git 
RUN git clone https://github.com/BinaryAnalysisPlatform/x86_64-binaries.git
RUN git clone https://github.com/baumane/capstone.git
RUN git clone https://github.com/KennethAdamMiller/multiverse.git
RUN git clone https://github.com/schieb/ELFManip
USER opam

##### SPEC CPU 2006
RUN pip install gdown
USER root
RUN /home/opam/.local/bin/gdown  https://drive.google.com/uc?id=1Pcqp7OfkTvlXn-rtIMDUPh4IjdBrixEs
RUN unzip -q cpu2006-103.zip && rm cpu2006-103.zip && rm -rf __MAXOSX result/* && cd cpu2006-103 && $(printf "yes\n") | ./install.sh
COPY Example-linux64-amd64-gcc41.cfg /home/opam/workspace/cpu2006-103/config/
RUN chown -R opam:opam /home/opam/.local/ && chown -R opam:opam $HOME/workspace
#WORKDIR /home/opam/workspace/cpu2006-103

##### multiverse
#USER opam
#python -m pip install --upgrade pyelftools==0.24 &&
RUN sudo apt-get update && sudo apt-get install libc6-dev-i386 wget -y && cd /home/opam/workspace/ && wget https://bootstrap.pypa.io/pip/2.7/get-pip.py && sudo python ./get-pip.py && rm ./get-pip.py && pip install setuptools_rust && pip install pwntools==3.12.2 && sudo pip uninstall capstone -y && cd /home/opam/workspace/ELFManip && sudo python setup.py install 
WORKDIR /home/opam/workspace/capstone
RUN ./make.sh && sudo ./make.sh install && cd bindings/python && sudo python setup.py install
WORKDIR /home/opam/workspace/multiverse
ENV TERM="xterm"
RUN make && echo "import curses; curses.setupterm();" > x64_asmblr.py && cat x64_assembler.py >> x64_asmblr.py && mv x64_asmblr.py x64_assembler.py

USER root
#TODO this should use git clone
RUN mkdir -p /home/opam/workspace/superset_disasm
COPY ./ /home/opam/workspace/superset_disasm/
RUN chown -R opam:opam /home/opam/workspace/superset_disasm/
USER opam


WORKDIR /home/opam/workspace/superset_disasm
ARG CACHEBUST
RUN rm setup.data ; eval `opam config env` ; opam depext --install bap-byteweight-frontend landmarks ; make clean ; make ; opam pin add superset_disasm ./ -y --use-internal-solver ; 

#How to be sure to trigger a fresh docker build based on 
#unit tests for superset_disasm executed and output saved.
#functional tests for superset_disasm executed and output saved.
