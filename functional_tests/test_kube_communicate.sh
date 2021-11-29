source scripts/tag.sh
export replicas=1
export test_size=1
export cores=1
cat ./configs/broker-job.yaml | envsubst | kubectl apply -f -
cat ./configs/broker-service.yaml | envsubst | kubectl apply -f -
cat ./configs/analysis-job.yaml | envsubst | kubectl apply -f -
kubectl wait --for=condition=complete --timeout=-1s job/analysis-${TAG}
if [[ $? -eq 0 ]]; then
	kubectl delete service broker-service-${TAG}
	kubectl delete job/analysis-${TAG}
fi
