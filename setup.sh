oasis setup -setup-update dynamic
ocaml setup.ml -configure --bindir $(opam config var bin)
pushd ${HOME}/workspace
[[ ! -d $ ./x86-binaries ]] git clone https://github.com/BinaryAnalysisPlatform/x86-binaries.git
[[ ! -d $ ./x86_64-binaries ]] git clone https://github.com/BinaryAnalysisPlatform/x86_64-binaries.git
[[ ! -d $ ./arm-binaries ]] git clone https://github.com/BinaryAnalysisPlatform/arm-binaries.git
popd
