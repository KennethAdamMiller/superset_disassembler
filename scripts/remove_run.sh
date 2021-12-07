if [[ -z ${TAG} ]]; then
	echo "TAG variable unset"
	exit
fi	
kubectl delete job/analysis-${TAG}
kubectl delete job/broker-${TAG}
kubectl delete service/broker-service-${TAG}
ksync delete cache-${TAG}
ksync delete corpus-${TAG}
