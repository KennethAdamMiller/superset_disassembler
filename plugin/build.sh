#pushd plugin/
rm -rf _build
#-pkg superset_disassemblers 
bapbuild -pkg bap-primus -pkg bap-knowledge -pkg superset_disassemblers superset_disassembler.plugin
#popd