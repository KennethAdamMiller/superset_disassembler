FROM binaryanalysisplatform/bap

USER root
RUN mkdir -p /home/opam/workspace/superset_disasm
COPY ./ /home/opam/workspace/superset_disasm/
RUN chown -R opam:opam /home/opam/workspace
USER opam

WORKDIR /home/opam/workspace/superset_disasm
RUN rm setup.data ; eval `opam config env` ; opam update ; opam install ounit fmt logs ; make clean ; make ; opam pin add superset_disasm ./ -y --use-internal-solver ; opam depext --install bap-byteweight-frontend

ARG testsize=999999
WORKDIR /home/opam/workspace
ENV PATH="/home/opam/.opam/4.09/bin:"$PATH

RUN sudo apt-get update && sudo apt-get install libc6-dev-i386 -y

RUN git clone https://github.com/BinaryAnalysisPlatform/x86-binaries.git 
RUN git clone https://github.com/BinaryAnalysisPlatform/x86_64-binaries.git
RUN git clone https://github.com/baumane/capstone.git
RUN git clone https://github.com/KennethAdamMiller/multiverse.git && make -C multiverse
RUN git clone https://github.com/schieb/ELFManip && cd ELFManip && sudo python setup.py install

##### SPEC CPU 2006
RUN pip install gdown
USER root
RUN chown -R opam:opam /home/opam/.local/
USER opam
RUN /home/opam/.local/bin/gdown  https://drive.google.com/uc?id=1Pcqp7OfkTvlXn-rtIMDUPh4IjdBrixEs
RUN unzip -q cpu2006-103.zip && rm cpu2006-103.zip && rm -rf __MAXOSX result/* && cd cpu2006-103 && $(printf "yes\n") | ./install.sh
COPY Example-linux64-amd64-gcc41.cfg /home/opam/workspace/cpu2006-103/config/
WORKDIR /home/opam/workspace/cpu2006-103

##### multiverse
#python -m pip install --upgrade pyelftools==0.24 &&
RUN sudo apt-get install wget && wget https://bootstrap.pypa.io/pip/2.7/get-pip.py && python ./get-pip.py && rm ./get-pip.py &&  pip install setuptools_rust && pip install pwntools==3.12.2 && sudo pip uninstall capstone -y
WORKDIR /home/opam/workspace/capstone
RUN ./make.sh && sudo ./make.sh install && cd bindings/python && sudo python setup.py install
WORKDIR /home/opam/workspace/multiverse
ENV TERM="xterm"
RUN echo "import curses; curses.setupterm();" > x64_asmblr.py && cat x64_assembler.py >> x64_asmblr.py && mv x64_asmblr.py x64_assembler.py
