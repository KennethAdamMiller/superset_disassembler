#for each node except the host apply the schronizer
for node in $(kubectl get nodes -o name)
do
	export TGT_HOST=$(basename ${node})
	if [[ "${TGT_HOST}" = "$(hostname)" ]]; then
		continue
	fi
	echo "deploying synchronize-${TGT_HOST}"
	cat ./configs/corpus_synchronizer.yaml | envsubst | kubectl apply -f -
done

for node in $(kubectl get pods -o name --selector=app=synchronizer) 
do
	kubectl wait --for=condition=ready ${node}
done

echo "Using devspace"
pushd /Volumes/corpus
for pod in $(kubectl get pods -o name --selector=app=synchronizer)
do
	devspace sync --container-path /Volumes/corpus --pod $(basename ${pod}) &
	disown
done
popd
pushd /Volumes/caches
for pod in $(kubectl get pods -o name --selector=app=synchronizer)
do
	devspace sync --container-path /Volumes/caches --pod $(basename ${pod}) &
	disown
done
popd
