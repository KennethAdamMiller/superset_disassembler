./scripts/get_binaries.sh
export test_size=$1
if [[ -z ${test_size} ]]; then
	export test_size=99999
fi
eval $(opam env)
bap recv_cache --perpetuate --bind_addr="tcp://*:9996" & 
recvr=$!
echo "Starting run_broker.py"
python3 scripts/run_broker.py ${test_size}
kill ${recvr}
plot_superset_cache
