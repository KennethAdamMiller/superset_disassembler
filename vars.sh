export OPAMSWITCH=4.10.0+flambda

export BAPVERSION=2.3.0
export USE_LANDMARKS=false
export USE_SPACETIME=false
if [[ "${USE_SPACETIME}" == "true" ]]; then
        export OPAM_SWITCH=4.10.0+spacetime
        export OCAML_SPACETIME_INTERVAL=1000
fi
