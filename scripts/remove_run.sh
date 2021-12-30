if [[ -z ${TAG} ]]; then
	echo "TAG variable unset"
	exit
fi	
kubectl delete deployment/analysis-${TAG}-${FSUFFIX} 
kubectl delete job/broker-${TAG}-${FSUFFIX}
kubectl delete service/broker-service-${TAG}-${FSUFFIX}
