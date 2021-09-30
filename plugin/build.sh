#pushd plugin/
rm -rf _build
#-pkg superset_disassemblers 
bapbuild -pkg findlib.dynload -pkg owl -pkg owl-plplot -pkg bap-primus -pkg bap-knowledge -pkg superset_disassemblers superset_disassembler.plugin
#popd
