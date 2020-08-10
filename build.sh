pushd src/
rm -rf _build
#-pkg superset_disassemblers 
bapbuild -pkg bap-primus -pkg bap-knowledge superset_disassembler.plugin
popd
