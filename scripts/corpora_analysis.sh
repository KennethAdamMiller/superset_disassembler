source scripts/tag.sh
source scripts/feature_suffix.sh
export test_size=999999
export cores=90
cat ./configs/broker-job.yaml | envsubst | kubectl apply -f -
cat ./configs/broker-service.yaml | envsubst | kubectl apply -f -
cat ./configs/analysis-deployment.yaml | envsubst | kubectl apply -f -
kubectl wait --for=condition=complete --timeout=120s job/analysis-${TAG}-${FSUFFIX}
kubectl wait --for=condition=complete --timeout=120s job/broker-${TAG}-${FSUFFIX}
if [[ $? -eq 0 ]]; then
	kubectl delete service broker-service-${TAG}-${FSUFFIX}
	kubectl delete job/analysis-${TAG}-${FSUFFIX}
	kubectl delete job/broker-${TAG}-${FSUFFIX}
fi
#build the latex document
