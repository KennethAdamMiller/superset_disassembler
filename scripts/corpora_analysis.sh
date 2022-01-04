export test_size=999999
./scripts/deploy.sh
kubectl wait --for=condition=complete --timeout=120s deployment/analysis-${TAG}-${FSUFFIX}
kubectl wait --for=condition=complete --timeout=120s job/broker-${TAG}-${FSUFFIX}
if [[ $? -eq 0 ]]; then
	kubectl delete service broker-service-${TAG}-${FSUFFIX}
	kubectl delete deployment/analysis-${TAG}-${FSUFFIX}
	kubectl delete job/broker-${TAG}-${FSUFFIX}
fi
#build the latex document
