export OPAM_SWITCH=4.11.0+flambda

export USE_LANDMARKS=false
export USE_SPACETIME=false
if [[ "${USE_SPACETIME}" == "true" ]]; then
        export OPAM_SWITCH=4.09.0+spacetime
        export OCAML_SPACETIME_INTERVAL=1000
fi
