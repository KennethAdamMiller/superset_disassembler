./scripts/get_binaries.sh
export test_size=$1
if [[ -z ${test_size} ]]; then
	export test_size=99999
fi
eval $(opam env)
python3 scripts/run_broker.py ${test_size}
plot_superset_cache
