pushd src/
rm -rf _build
#-pkg superset_disassemblers 
bapbuild -pkg bap-primus superset_disassembler.plugin
popd
