./scripts/get_binaries.sh
export test_size=$1
eval $(opam env)
echo "Starting run_broker.py"
python3 scripts/run_broker.py ${test_size}
#broker=$!
#bap recv_cache --perpetuate --bind_addr="tcp://*:9996" & 
#recvr=$!
#wait ${broker}
#kill ${recvr}
#plot_superset_cache
